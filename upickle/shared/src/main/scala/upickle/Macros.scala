package upickle

import derive.ScalaVersionStubs._
import scala.annotation.StaticAnnotation
import scala.language.experimental.macros
import compat._
import derive._
import acyclic.file
import language.higherKinds
import language.existentials

/**
 * Implementation of macros used by uPickle to serialize and deserialize
 * case classes automatically. You probably shouldn't need to use these
 * directly, since they are called implicitly when trying to read/write
 * types you don't have a Reader/Writer in scope for.
 */
object Macros {
  abstract class Reading extends Derive{
    val c: Context
    import c.universe._
    def wrapObject(t: c.Tree) = q"${c.prefix}.SingletonR($t)"
    def wrapCase0(t: c.Tree, targetType: c.Type) =
      q"${c.prefix}.${newTermName("Case0R")}($t.apply _: () => $targetType)"
    def wrapCase1(t: c.Tree,
                  arg: String,
                  default: c.Tree,
                  typeArgs: Seq[c.Type],
                  argType: c.Type,
                  targetType: c.Type) = {
      q"""
        ${c.prefix}.CaseR[_root_.scala.Tuple1[$argType], $targetType](
          _ match {case _root_.scala.Tuple1(x) => $t.apply[..$typeArgs](x)},
          _root_.scala.Array($arg),
          _root_.scala.Array($default)
        )(${c.prefix}.Tuple1R)
        """
    }
    def wrapCaseN(t: c.Tree,
                  args: Seq[String],
                  defaults: Seq[c.Tree],
                  typeArgs: Seq[c.Type],
                  argTypes: Seq[Type],
                  targetType: c.Type) = {
      val x = q"$freshName"
      val name = newTermName("Tuple"+args.length+"R")
      val argSyms = (1 to args.length).map(t => q"$x.${newTermName("_"+t)}")
      q"""
        ${c.prefix}.CaseR[(..$argTypes), $targetType](
          ($x: (..$argTypes)) => ($t.apply: (..$argTypes) => $targetType)(..$argSyms),
          _root_.scala.Array(..$args),
          _root_.scala.Array(..$defaults)
        )(${c.prefix}.$name)
      """
    }
    def mergeTrait(subtrees: Seq[Tree], subtypes: Seq[Type], targetType: c.Type): Tree = {
      val merged =
        subtrees.map(p => q"$p.read": Tree)
          .reduce((a, b) => q"$a orElse $b")
      q"${c.prefix}.Reader[$targetType]($merged)"
    }
    def knot(t: Tree) = q"${c.prefix}.Knot.Reader(() => $t)"

  }
  abstract class Writing extends Derive{
    val c: Context
    import c.universe._
    def wrapObject(obj: c.Tree) = q"${c.prefix}.SingletonW($obj)"
    def wrapCase0(companion: c.Tree, targetType: c.Type) = q"${c.prefix}.${newTermName("Case0W")}($companion.unapply)"
    def findUnapply(tpe: Type) = {
      val (companion, paramTypes, argSyms) = getArgSyms(tpe).fold(
        errMsg => c.abort(c.enclosingPosition, errMsg),
        x => x
      )
      Seq("unapply", "unapplySeq")
        .map(newTermName(_))
        .find(companion.tpe.member(_) != NoSymbol)
        .getOrElse(c.abort(c.enclosingPosition, "None of the following methods " +
        "were defined: unapply, unapplySeq"))
    }
    def wrapCase1(companion: c.Tree,
                  arg: String,
                  default: c.Tree,
                  typeArgs: Seq[c.Type],
                  argType: Type,
                  targetType: c.Type) = q"""
        ${c.prefix}.CaseW[_root_.scala.Tuple1[$argType], $targetType](
          $companion.${findUnapply(targetType)}(_).map(_root_.scala.Tuple1.apply),
          _root_.scala.Array($arg),
          _root_.scala.Array($default)
        )(${c.prefix}.Tuple1W)
        """
    def internal = q"${c.prefix}.Internal"
    def wrapCaseN(companion: c.Tree,
                  args: Seq[String],
                  defaults: Seq[c.Tree],
                  typeArgs: Seq[c.Type],
                  argTypes: Seq[Type],
                  targetType: c.Type) = {
      val name = newTermName("Tuple"+args.length+"W")
      q"""
        ${c.prefix}.CaseW[(..$argTypes), $targetType](
          $companion.${findUnapply(targetType)}[..$typeArgs],
          _root_.scala.Array(..$args),
          _root_.scala.Array(..$defaults)
        )(${c.prefix}.$name)
      """
    }
    def mergeTrait(subtree: Seq[Tree], subtypes: Seq[Type], targetType: c.Type): Tree = {
      val merged =
        if (subtree.length == 1) q"$internal.merge0(${subtree(0)}.write)"
        else subtree.map(p => q"$p.write": Tree)
          .reduce((a, b) => q"$internal.merge($a, $b)")
      q"${c.prefix}.Writer[$targetType]($merged)"
    }
    def knot(t: Tree) = q"${c.prefix}.Knot.Writer(() => $t)"
  }
  def macroRImpl[T, R[_]](c0: Context)
                         (implicit e1: c0.WeakTypeTag[T], e2: c0.WeakTypeTag[R[_]]): c0.Expr[R[T]] = {
    import c0.universe._

    val res = new Reading{
      val c: c0.type = c0

      def typeclassFor[T: this.c.WeakTypeTag]: this.c.universe.Type = c.weakTypeOf[R[T]]
    }.derive[T]
    c0.Expr[R[T]](res)
  }

  def macroWImpl[T, W[_]](c0: Context)
                         (implicit e1: c0.WeakTypeTag[T], e2: c0.WeakTypeTag[W[_]]): c0.Expr[W[T]] = {
    import c0.universe._

    val res = new Writing{
      val c: c0.type = c0
      def typeclassFor[T: this.c.WeakTypeTag]: this.c.universe.Type = c.weakTypeOf[W[T]]
    }.derive[T]
//    println(res)
    c0.Expr[W[T]](res)
  }
  
}

