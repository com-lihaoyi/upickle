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
  abstract class Reading[M[_]] extends Derive[M]{
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
    def mergeTrait(subtree: Seq[Tree], subtypes: Seq[Type], targetType: c.Type): Tree = {
      val merged =
        ts.map(p => q"$p.read": Tree)
          .reduce((a, b) => q"$a orElse $b")
      q"${c.prefix}.Reader[$targetType]($merged)"
    }
    def knot(t: Tree) = q"${c.prefix}.Knot.Reader(() => $t)"

  }
  abstract class Writing[M[_]] extends Derive[M]{
    val c: Context
    import c.universe._
    def wrapObject(t: c.Tree) = q"${c.prefix}.${newTermName("SingletonW")}($t)"
    def wrapCase0(t: c.Tree, targetType: c.Type) = q"${c.prefix}.${newTermName("Case0W")}($t.unapply)"
    def findUnapply(tpe: Type) = {
      val (companion, paramTypes, argSyms) = getArgSyms(tpe)
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
    def internal = q"${c.prefix}.Internal"
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
    def mergeTrait(subtree: Seq[Tree], subtypes: Seq[Type], targetType: c.Type): Tree = {
      val merged =
        if (ts.length == 1) q"$internal.merge0(${ts(0)}.write)"
        else ts.map(p => q"$p.write": Tree)
          .reduce((a, b) => q"$internal.merge($a, $b)")
      q"${c.prefix}.Writer[$targetType]($merged)"
    }
    def knot(t: Tree) = q"${c.prefix}.Knot.Writer(() => $t)"
  }
  def macroRImpl[T, R[_]](c0: Context)
                         (implicit e1: c0.WeakTypeTag[T], e2: c0.WeakTypeTag[R[_]]): c0.Expr[R[T]] = {
    import c0._
    import c0.universe._

    val res = new Reading[R]{
      val c: c0.type = c0
      def typeclass = e2
    }.derive[T]
//    println(res)
    c0.Expr[R[T]](res)
  }

  def macroWImpl[T, W[_]](c0: Context)
                            (implicit e1: c0.WeakTypeTag[T], e2: c0.WeakTypeTag[W[_]]): c0.Expr[W[T]] = {
    import c0._
    import c0.universe._

    val res = new Writing[W]{
      val c: c0.type = c0
      def typeclass = e2
    }.derive[T]
//    println(res)
    c0.Expr[W[T]](res)
  }
  import language.experimental._
  import reflect.macros.blackbox.Context
  def macroImpl[T](c: Context)(implicit e: c.WeakTypeTag[T]): c.Expr[String] = {
    import c.universe._
    c.Expr[String](q"${e.toString}")
  }
  def m = macro macroImpl[Seq[Int]]
}


