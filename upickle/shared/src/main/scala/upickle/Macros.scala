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



  object R extends Derive.Config("R", "Reader", Seq("apply"), false)

  object W extends Derive.Config("W", "Writer", Seq("unapply", "unapplySeq"), true)


  def macroRImpl[T, R[_]](c0: Context)(implicit e1: c0.WeakTypeTag[T], e2: c0.WeakTypeTag[R[_]]): c0.Expr[R[T]] = {
    import c0._
    import c0.universe._
    val res = new Derive(R){val c: c0.type = c0}.derive[T](
      _.map(p => q"$p.read": Tree)
      .reduce((a, b) => q"$a orElse $b")
    )(implicitly[c0.WeakTypeTag[T]])
    c0.Expr[R[T]](res)
  }

  def macroWImpl[T, W[_]](c0: Context)(implicit e1: c0.WeakTypeTag[T], e2: c0.WeakTypeTag[W[_]]): c0.Expr[W[T]] = {
    import c0._
    import c0.universe._
    def internal = q"${c0.prefix}.Internal"
    val res = new Derive(W){val c: c0.type = c0}.derive[T](
      things =>
        if (things.length == 1) q"$internal.merge0(${things(0)}.write)"
        else things.map(p => q"$p.write": Tree)
          .reduce((a, b) => q"$internal.merge($a, $b)")
    )(implicitly[c0.WeakTypeTag[T]])
    c0.Expr[W[T]](res)
  }
}


