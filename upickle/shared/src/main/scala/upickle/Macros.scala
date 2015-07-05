package upickle

import derive.ScalaVersionStubs._
import scala.annotation.StaticAnnotation
import scala.language.experimental.macros
import compat._
import derive._
import acyclic.file


/**
 * Implementation of macros used by uPickle to serialize and deserialize
 * case classes automatically. You probably shouldn't need to use these
 * directly, since they are called implicitly when trying to read/write
 * types you don't have a Reader/Writer in scope for.
 */
object Macros {
  object R extends Derive.Config(
    "Reader"
  )

  object W extends Derive.Config(
    "Writer"
  )

  abstract class Reading extends Derive(R){
    val c: Context
    import c.universe._
    def wrapObject(t: c.Tree) = q"${c.prefix}.${newTermName("SingletonR")}($t)"
    def wrapCase0(t: c.Tree, targetType: c.Type) =
      q"${c.prefix}.${newTermName("Case0R")}($t.apply _: () => $targetType)"
    def wrapCase1(t: c.Tree,
                  arg: String,
                  default: c.Tree,
                  typeArgs: Seq[c.Type],
                  argType: c.Type,
                  targetType: c.Type) = {
      q"""
        ${c.prefix}.CaseR[Tuple1[$argType], $targetType](
          _ match {case Tuple1(x) => $t.apply[..$typeArgs](x)},
          Array($arg),
          Array($default)
        )
        """
    }
    def wrapCaseN(t: c.Tree,
                  args: Seq[String],
                  defaults: Seq[c.Tree],
                  typeArgs: Seq[c.Type],
                  argTypes: Seq[Type],
                  targetType: c.Type) = {
      val argSyms = (1 to args.length).map(t => q"xyz123.${newTermName("_"+t)}")
      q"""
        ${c.prefix}.CaseR[(..$argTypes), $targetType](
          xyz123 => ($t.apply: (..$argTypes) => $targetType)(..$argSyms),
          Array(..$args),
          Array(..$defaults)
        )
      """
    }
  }
  abstract class Writing extends Derive(W){
    val c: Context
    import c.universe._
    def wrapObject(t: c.Tree) = q"${c.prefix}.${newTermName("SingletonW")}($t)"
    def wrapCase0(t: c.Tree, targetType: c.Type) = q"${c.prefix}.${newTermName("Case0W")}($t.unapply)"
    def findUnapply(tpe: Type) = {
      val (companion, paramTypes, argSyms) = getArgSyms(tpe)
      println("findUnapply " + tpe)
      Seq("unapply", "unapplySeq")
        .map(newTermName(_))
        .find(companion.tpe.member(_) != NoSymbol)
        .getOrElse(c.abort(c.enclosingPosition, "None of the following methods " +
        "were defined: unapply, unapplySeq"))
    }
    def wrapCase1(t: c.Tree,
                  arg: String,
                  default: c.Tree,
                  typeArgs: Seq[c.Type],
                  argType: Type,
                  targetType: c.Type) = q"""
        ${c.prefix}.CaseW[Tuple1[$argType], $targetType](
          $t.${findUnapply(targetType)}(_).map(Tuple1.apply),
          Array($arg),
          Array($default)
        )
        """
    def wrapCaseN(t: c.Tree,
                  args: Seq[String],
                  defaults: Seq[c.Tree],
                  typeArgs: Seq[c.Type],
                  argTypes: Seq[Type],
                  targetType: c.Type) = q"""
        ${c.prefix}.CaseW[(..$argTypes), $targetType](
          $t.${findUnapply(targetType)}[..$typeArgs],
          Array(..$args),
          Array(..$defaults)
        )
      """

  }
  def macroRImpl[T, R[_]](c0: Context)(implicit e1: c0.WeakTypeTag[T], e2: c0.WeakTypeTag[R[_]]): c0.Expr[R[T]] = {
    import c0._
    import c0.universe._

    val res = new Reading{val c: c0.type = c0}.derive[T](
      _.map(p => q"$p.read": Tree)
        .reduce((a, b) => q"$a orElse $b")
    )(implicitly[c0.WeakTypeTag[T]])
//    println(res)
    c0.Expr[R[T]](res)
  }

  def macroWImpl[T, W[_]](c0: Context)(implicit e1: c0.WeakTypeTag[T], e2: c0.WeakTypeTag[W[_]]): c0.Expr[W[T]] = {
    import c0._
    import c0.universe._
    def internal = q"${c0.prefix}.Internal"
    val res = new Writing{val c: c0.type = c0}.derive[T](
      things =>
        if (things.length == 1) q"$internal.merge0(${things(0)}.write)"
        else things.map(p => q"$p.write": Tree)
          .reduce((a, b) => q"$internal.merge($a, $b)")
    )(implicitly[c0.WeakTypeTag[T]])
//    println(res)
    c0.Expr[W[T]](res)
  }
}


