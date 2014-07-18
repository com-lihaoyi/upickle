package upickle

import scala.reflect.macros.whitebox._
import scala.Some

/**
 * Created by haoyi on 7/8/14.
 */
object Macros {
  def macroRWImpl[T: c.WeakTypeTag](c: Context) = {
    import c.universe._
    val z = picklerFor(c)(weakTypeTag[T].tpe)

    c.Expr[RW[T]](z)
  }

  def picklerFor(c: Context)(tpe: c.Type): c.Tree = {
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

    println()

    println(tpe)
    tpe.decl(nme.CONSTRUCTOR) match {
      case NoSymbol if clsSymbol.isSealed => // I'm a sealed trait/class!
        val subPicklers =
          for(subCls <- clsSymbol.knownDirectSubclasses) yield {
            picklerFor(c)(subCls.asType.toType)
          }
        val writes = subPicklers.map(p => q"$p.write")
                                .reduceLeft[Tree]((a, b) => q"$a merge $b")

        val reads = subPicklers.map(p => q"$p.read")
                               .reduceLeft[Tree]((a, b) => q"$a orElse $b")
        val z = q"""
          upickle.Implicits.knotRW{implicit i: upickle.RWKnot[$tpe] =>
            new upickle.ReadWriter[$tpe](
              $writes,
              upickle.Implicits.validate("Sealed"){
                $reads
              }
            )
          }
        """
        println(z)
        println("SealedSomething")

//        c.typecheck(z, withMacrosDisabled = true)
        z
      case x if tpe.typeSymbol.isModuleClass =>
        val mod = tpe.typeSymbol.asClass.module

        val z  = annotate(q"upickle.Implicits.Case0ReadWriter[$mod.type]($mod)")
        println("Object")

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
          val rwName = TermName(s"Case${args.length}ReadWriter")
          val name = TermName(tpe.typeSymbol.name.toString)

          q"upickle.Implicits.$rwName($name.apply, $name.unapply, Seq(..$args)): upickle.ReadWriter[$tpe]"
        }
//        println(pickler)


        val z = annotate(pickler)
        println("Class")

        z
    }
  }
}
