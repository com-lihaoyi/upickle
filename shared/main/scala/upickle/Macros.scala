package upickle

import scala.reflect.macros._
import scala.reflect._
import scala.annotation.{ClassfileAnnotation, StaticAnnotation}
import scala.language.experimental.macros

//import acyclic.file
/**
 * Used to annotate either case classes or their fields, telling uPickle
 * to use a custom string as the key for that class/field rather than the
 * default string which is the full-name of that class/field.
 */
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
    assert(!tpe.typeSymbol.fullName.startsWith("scala."))
    val res = c.Expr[Reader[T]]{
      val x = picklerFor(c)(tpe, RW.R)(
        _.map(p => q"$p.read": Tree)
         .reduce((a, b) => q"$a orElse $b")
      )

      val msg = "Tagged Object " + tpe.typeSymbol.fullName
      q"""upickle.Internal.validateReader($msg){$x}"""
    }
//    println(res)
    res
  }
  def macroWImpl[T: c.WeakTypeTag](c: Context) = {
    import c.universe._
    val tpe = weakTypeTag[T].tpe
    assert(!tpe.typeSymbol.fullName.startsWith("scala."))
//    println("macroWImpl " + tpe)
    val res = c.Expr[Writer[T]]{
      picklerFor(c)(tpe, RW.W) { things =>
        if (things.length == 1){
          q"upickle.Internal.merge0(${things(0)}.write)"
        }else{
          things.map(p => q"$p.write": Tree)
                .reduce((a, b) => q"upickle.Internal.merge($a, $b)")
        }
      }
    }
//    println(res)
    res
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

  def getCompanion(c: Context)(tpe: c.Type) = {
    import c.universe._
    val symTab = c.universe.asInstanceOf[reflect.internal.SymbolTable]
    val pre = tpe.asInstanceOf[symTab.Type].prefix.asInstanceOf[Type]
    c.universe.treeBuild.mkAttributedRef(pre, tpe.typeSymbol.companionSymbol)
  }
  /**
   * Generates a pickler for a particuler Type
   *
   * @param tpe The type we are generating the pickler for
   * @param rw Configuration that determines whether it's a Reader or
   *           a Writer, together with the various names wihich vary
   *           between those two choices
   * @param treeMaker How to merge the trees of the multiple subpicklers
   *                  into one larger tree
   */
  def picklerFor(c: Context)
                (tpe: c.Type, rw: RW)
                (treeMaker: Seq[c.Tree] => c.Tree): c.Tree = {

    import c.universe._
//    println("picklerFor " + tpe)
    val clsSymbol = tpe.typeSymbol.asClass

    def annotate(pickler: Tree) = {
      val sealedParent = tpe.baseClasses.find(_.asClass.isSealed)
      sealedParent.fold(pickler){ parent =>
        val index = customKey(c)(tpe.typeSymbol).getOrElse(tpe.typeSymbol.fullName)
        q"upickle.Internal.annotate($pickler, $index)"
      }
    }

    val pick = tpe.declaration(nme.CONSTRUCTOR) match {
      case NoSymbol if clsSymbol.isSealed => // I'm a sealed trait/class!
        val subPicklers =
          for(subCls <- clsSymbol.knownDirectSubclasses.toSeq) yield {
            picklerFor(c)(subCls.asType.toType, rw)(treeMaker)
          }
        val combined = treeMaker(subPicklers)

        q"""upickle.${newTermName(rw.long)}[$tpe]($combined)"""

      case x if tpe.typeSymbol.isModuleClass =>
        val mod = tpe.typeSymbol.asClass.module
        annotate(q"upickle.Internal.${newTermName("Case0"+rw.short)}[${tpe}]($mod)")

      case x => // I'm a class

        val pickler = {

          val companion = getCompanion(c)(tpe)

          val argSyms =
            companion.tpe
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
            companion.tpe.member(defaultName) match{
              case NoSymbol => q"null"
              case x => q"upickle.writeJs($companion.$defaultName)"
            }
          }
          if (args.length == 0) // 0-arg case classes are treated like `object`s
            q"upickle.Internal.${newTermName("Case0"+rw.short)}($companion())"
          else if (args.length == 1 && rw == RW.W) // 1-arg case classes need their output wrapped in a Tuple1
            q"upickle.Internal.$rwName(x => $companion.$actionName(x).map(Tuple1.apply), Array(..$args), Array(..$defaults)): upickle.${newTypeName(rw.long)}[$tpe]"
          else // Otherwise, reading and writing are kinda identical
            q"upickle.Internal.$rwName($companion.$actionName, Array(..$args), Array(..$defaults)): upickle.${newTypeName(rw.long)}[$tpe]"
        }

        annotate(pickler)
    }
    val knotName = newTermName("knot"+rw.short)
    val i = c.fresh[TermName]("i")
    val x = c.fresh[TermName]("x")
    q"""
       upickle.Internal.$knotName{implicit $i: upickle.Knot.${newTypeName(rw.short)}[$tpe] =>
          val $x = $pick
          $i.copyFrom($x)
          $x
        }
     """
  }
}
 
