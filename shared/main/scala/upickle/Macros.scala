package upickle

import scala.reflect.macros.whitebox._
import scala.Some
import scala.reflect.macros.TypecheckException

/**
 * Created by haoyi on 7/8/14.
 */
object Macros {
  def macroRImpl[T: c.WeakTypeTag](c: Context) = {
    import c.universe._

    val z: Tree = try {
      c.inferImplicitValue(weakTypeOf[Reader[T]], silent = false, withMacrosDisabled = true)

    } catch {
      case e: TypecheckException =>
        val tpe = weakTypeTag[T].tpe

        picklerFor(c)(tpe, "Reader") { subPicklers =>
          val reads = subPicklers.map(p => q"$p.read")
            .reduce[Tree]((a, b) => q"$a orElse $b")
          q"""
          upickle.Implicits.knotR{implicit i: upickle.Knot.R[$tpe] =>
            val x = new upickle.ReaderCls[$tpe](upickle.Generated.validate("Sealed"){$reads})
            i.copyFrom(x)
            x
          }
        """

        }
    }
//    println(z)
    c.Expr[R[T]](z)
  }
  def macroWImpl[T: c.WeakTypeTag](c: Context) = {
    import c.universe._

    val z: Tree = try {
      c.inferImplicitValue(weakTypeOf[Writer[T]], silent = false, withMacrosDisabled = true)
    }catch {case e: TypecheckException =>
      val tpe = weakTypeTag[T].tpe
      picklerFor(c)(tpe, "Writer"){ subPicklers =>
        val writes = subPicklers.map(p => q"$p.write")
          .reduce[Tree]((a, b) => q"$a merge $b")

        q"""
          upickle.Implicits.knotW{implicit i: upickle.Knot.W[$tpe] =>
            val x = new upickle.WriterCls[$tpe]($writes)
            i.copyFrom(x)
            x
          }
        """

      }
    }

//    println(z)
    c.Expr[W[T]](z)
  }

  def picklerFor(c: Context)(tpe: c.Type, name: String)(treeMaker: Seq[c.Tree] => c.Tree): c.Tree = {
    import c.universe._
    val clsSymbol = tpe.typeSymbol.asClass
    def annotate(pickler: Tree) = {
      val sealedParent = tpe.baseClasses.find(_.asClass.isSealed)
      sealedParent match {
        case Some(parent) =>
          val index =
            parent.asClass
              .knownDirectSubclasses
              .toSeq
              .sortBy(_.fullName)
              .indexWhere(_.fullName == tpe.typeSymbol.fullName)
              .toString
          q"annotate($pickler, $index)"
        case None => pickler
      }
    }

//    println()
//
//    println(tpe)
    tpe.decl(nme.CONSTRUCTOR) match {
      case NoSymbol if clsSymbol.isSealed => // I'm a sealed trait/class!
        val subPicklers =
          for(subCls <- clsSymbol.knownDirectSubclasses.toSeq) yield {
            picklerFor(c)(subCls.asType.toType, name)(treeMaker)
          }

        val z = treeMaker(subPicklers)

//        println(z)
//        println("SealedSomething")
//        Thread.sleep(1000)
        z
      case x if tpe.typeSymbol.isModuleClass =>
        val mod = tpe.typeSymbol.asClass.module

        val z  = annotate(q"upickle.Implicits.${TermName("Case0"+name)}[$mod.type]($mod)")
//        println("Object")

        z
      case x => // I'm a class

        val pickler = {
          val args =
            tpe.typeSymbol
               .companion
               .info
               .member(TermName("apply"))
               .typeSignature
               .paramLists
               .flatten
               .map(_.name.toString)
          val rwName = TermName(s"Case${args.length}$name")
          val className = TermName(tpe.typeSymbol.name.toString)
          val actionName = TermName(if (name == "Writer") "unapply" else "apply")

          if (args.length == 1 && name == "Writer")
            q"upickle.Generated.$rwName(x => $className.$actionName(x).map(Tuple1.apply), Seq(..$args)): upickle.${TypeName(name)}[$tpe]"
          else
            q"upickle.Generated.$rwName($className.$actionName, Seq(..$args)): upickle.${TypeName(name)}[$tpe]"
        }
//        println(pickler)


        val z = annotate(pickler)
//        println("Class")
//        println(z)
        z
    }
  }
}
