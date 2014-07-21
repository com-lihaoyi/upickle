package upickle

import scala.reflect.macros._
import scala.reflect._
import scala.annotation.{ClassfileAnnotation, StaticAnnotation}

//import acyclic.file

class key(s: String) extends StaticAnnotation

/**
 * Implementation of macros used by uPickle to serialize and deserialize
 * case classes automatically. You probably shouldn't need to use these
 * directly, since they are called implicitly when trying to read/write
 * types you don't have a Reader/Writer in scope for.
 */
object Macros {
  def macroRImpl[T: c.WeakTypeTag](c: Context) = {

    import c.universe._

    val z: Tree = try {
      c.inferImplicitValue(weakTypeOf[Reader[T]], silent = false, withMacrosDisabled = true)

    } catch {
      case e: TypecheckException =>
        val tpe = weakTypeTag[T].tpe

        picklerFor(c)(tpe, "R", "Reader") { (tpe, subPicklers) =>
          val reads = subPicklers.map(p => q"$p.read")
            .reduce[Tree]((a, b) => q"$a orElse $b")
          q"""
          upickle.Internal.knotR{implicit i: upickle.Knot.R[$tpe] =>
            val x = upickle.Reader[$tpe](upickle.validate("Sealed"){$reads})
            i.copyFrom(x)
            x
          }
        """

        }
    }
//    println(z)
    c.Expr[Reader[T]](z)
  }
  def macroWImpl[T: c.WeakTypeTag](c: Context) = {
    import c.universe._

    val z: Tree = try {
      c.inferImplicitValue(weakTypeOf[Writer[T]], silent = false, withMacrosDisabled = true)
    }catch {case e: TypecheckException =>
      val tpe = weakTypeTag[T].tpe
      picklerFor(c)(tpe, "W", "Writer"){ (tpe, subPicklers) =>
        val writes = subPicklers.map(p => q"$p.write")
          .reduce[Tree]((a, b) => q"upickle.Internal.mergeable($a) merge $b")

        q"""
          upickle.Internal.knotW{implicit i: upickle.Knot.W[$tpe] =>

            val x = upickle.Writer[$tpe]($writes)
            i.copyFrom(x)
            x
          }
        """

      }
    }

//    println(z)
    c.Expr[Writer[T]](z)
  }
  def customKey(c: Context)(sym: c.Symbol): Option[String] = {
    import c.universe._
    sym.annotations
       .find(_.tpe == typeOf[key])
       .flatMap(_.scalaArgs.headOption)
       .map{case Literal(Constant(s)) => s.toString}
  }
  def picklerFor(c: Context)(tpe: c.Type, name: String, longName: String)(treeMaker: (c.Type, Seq[c.Tree]) => c.Tree): c.Tree = {
    import c.universe._
    val clsSymbol = tpe.typeSymbol.asClass
    def annotate(pickler: Tree) = {

      val sealedParent = tpe.baseClasses.find(_.asClass.isSealed)
      sealedParent match {
        case Some(parent) =>
          val index = customKey(c)(tpe.typeSymbol).getOrElse(tpe.typeSymbol.fullName)

          q"upickle.Internal.annotate($pickler, $index)"
        case None => pickler
      }
    }

//    println()
//
//    println(tpe)

    tpe.declaration(nme.CONSTRUCTOR) match {
      case NoSymbol if clsSymbol.isSealed => // I'm a sealed trait/class!
        val subPicklers =
          for(subCls <- clsSymbol.knownDirectSubclasses.toSeq) yield {
            picklerFor(c)(subCls.asType.toType, name, longName)(treeMaker)
          }

        val z = treeMaker(tpe, subPicklers)

//        println(z)
//        println("SealedSomething")
//        Thread.sleep(1000)
        z
      case x if tpe.typeSymbol.isModuleClass =>
        val mod = tpe.typeSymbol.asClass.module
//        println("XXX")
//        println(mod)


        val z  = annotate(q"upickle.Internal.${newTermName("Case0"+name)}($mod)")
//        println("Object")

        z
      case x => // I'm a class

        val pickler = {
          val companion =
            tpe.typeSymbol
               .companionSymbol
          val argSyms =
            companion
               .typeSignature
               .member(newTermName("apply"))
               .asMethod
               .paramss
               .flatten

          val args = argSyms.map { p =>
            customKey(c)(p).getOrElse(p.name.toString)
          }

          val rwName = newTermName(s"Case${args.length}$name")
          val className = newTermName(tpe.typeSymbol.name.toString)
          val actionName = newTermName(if (name == "W") "unapply" else "apply")
          val defaults = argSyms.zipWithIndex.map{ case (s, i) =>
            val defaultName = newTermName("apply$default$" + (i + 1))
            companion.typeSignature.member(defaultName) match{
              case NoSymbol => q"null"
              case x => q"upickle.writeJs($companion.$defaultName)"
            }
          }
          if (args.length == 0)
            q"upickle.Internal.${newTermName("Case0"+name)}($className())"
          else if (args.length == 1 && name == "W")
            q"upickle.Internal.$rwName(x => $className.$actionName(x).map(Tuple1.apply), Seq(..$args), Seq(..$defaults)): upickle.${newTypeName(longName)}[$tpe]"
          else if(name == "W")
            q"upickle.Internal.$rwName($className.$actionName, Seq(..$args), Seq(..$defaults)): upickle.${newTypeName(longName)}[$tpe]"
          else // name == "R"
            q"upickle.Internal.$rwName($className.$actionName, Seq(..$args), Seq(..$defaults)): upickle.${newTypeName(longName)}[$tpe]"
        }


        val z = annotate(pickler)
//        println("Class")
//        println(z)
        z
    }
  }
}
