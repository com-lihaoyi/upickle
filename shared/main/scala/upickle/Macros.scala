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
  class RW(val short: String, val long: String, val actionName: String)
  object RW{
    object R extends RW("R", "Reader", "apply")
    object W extends RW("W", "Writer", "unapply")
  }
  def macroRImpl[T: c.WeakTypeTag](c: Context) = {
    import c.universe._
    val tpe = weakTypeTag[T].tpe
    c.Expr[Reader[T]]{
      picklerFor(c)(tpe, RW.R) { (tpe, subPicklers) =>
        val reads = subPicklers.map(p => q"$p.read": Tree)
                               .reduce((a, b) => q"$a orElse $b")
        q"""
        upickle.Internal.knotR{implicit i: upickle.Knot.R[$tpe] =>
          val x = upickle.Reader[$tpe](upickle.validate("CaseClass"){$reads})
          i.copyFrom(x)
          x
        }
      """
      }
    }
  }
  def macroWImpl[T: c.WeakTypeTag](c: Context) = {
    import c.universe._
    val tpe = weakTypeTag[T].tpe
    c.Expr[Writer[T]]{
      picklerFor(c)(tpe, RW.W){ (tpe, subPicklers) =>
        val writes = subPicklers.map(p => q"$p.write": Tree)
                                .reduce((a, b) => q"upickle.Internal.mergeable($a) merge $b")
        q"""
          upickle.Internal.knotW{implicit i: upickle.Knot.W[$tpe] =>
            val x = upickle.Writer[$tpe]($writes)
            i.copyFrom(x)
            x
          }
        """
      }
    }
  }

  /**
   * Get the custom @key annotation from
   * the parameter Symbol if it exists
   */
  def customKey(c: Context)(sym: c.Symbol): Option[String] = {
    import c.universe._
    sym.annotations
       .find(_.tpe == typeOf[key])
       .flatMap(_.scalaArgs.headOption)
       .map{case Literal(Constant(s)) => s.toString}
  }

  def picklerFor(c: Context)
                (tpe: c.Type, rw: RW)
                (treeMaker: (c.Type, Seq[c.Tree]) => c.Tree): c.Tree = {

    import c.universe._
    val clsSymbol = tpe.typeSymbol.asClass

    def annotate(pickler: Tree) = {
      val sealedParent = tpe.baseClasses.find(_.asClass.isSealed)
      sealedParent.fold(pickler){ parent =>
        val index = customKey(c)(tpe.typeSymbol).getOrElse(tpe.typeSymbol.fullName)
        q"upickle.Internal.annotate($pickler, $index)"
      }
    }

    tpe.declaration(nme.CONSTRUCTOR) match {
      case NoSymbol if clsSymbol.isSealed => // I'm a sealed trait/class!
        val subPicklers =
          for(subCls <- clsSymbol.knownDirectSubclasses.toSeq) yield {
            picklerFor(c)(subCls.asType.toType, rw)(treeMaker)
          }

        treeMaker(tpe, subPicklers)

      case x if tpe.typeSymbol.isModuleClass =>
        val mod = tpe.typeSymbol.asClass.module
        annotate(q"upickle.Internal.${newTermName("Case0"+rw.short)}($mod)")

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

          val rwName = newTermName(s"Case${args.length}${rw.short}")
          val className = newTermName(tpe.typeSymbol.name.toString)
          val actionName = newTermName(rw.actionName)
          val defaults = argSyms.zipWithIndex.map{ case (s, i) =>
            val defaultName = newTermName("apply$default$" + (i + 1))
            companion.typeSignature.member(defaultName) match{
              case NoSymbol => q"null"
              case x => q"upickle.writeJs($companion.$defaultName)"
            }
          }
          if (args.length == 0) // 0-arg case classes are treated like `object`s
            q"upickle.Internal.${newTermName("Case0"+rw.short)}($className())"
          else if (args.length == 1 && rw == RW.W) // 1-arg case classes need their output wrapped in a Tuple1
            q"upickle.Internal.$rwName(x => $companion.$actionName(x).map(Tuple1.apply), Array(..$args), Array(..$defaults)): upickle.${newTypeName(rw.long)}[$tpe]"
          else // Otherwise, reading and writing are kinda identical
            q"upickle.Internal.$rwName($companion.$actionName, Array(..$args), Array(..$defaults)): upickle.${newTypeName(rw.long)}[$tpe]"
        }

        annotate(pickler)
    }
  }
}
