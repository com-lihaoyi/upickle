package upickle
import language.experimental.macros
/**
* Created by haoyi on 7/4/15.
*/
class Api extends Types with Implicits with Generated with LowPriX{
  protected[this] def validate[T](name: String)(pf: PartialFunction[Js.Value, T]) = Internal.validate(name)(pf)

  type key = derive.key
}
object old extends Api
object Forwarder{
  def applyR[T](c: derive.ScalaVersionStubs.Context)
              (implicit e: c.WeakTypeTag[T]): c.Expr[T] = {
    import c.universe._
    c.Expr[T](q"${c.prefix}.macroR0[$e, upickle.old.Reader]")
  }
  def applyW[T](c: derive.ScalaVersionStubs.Context)
              (implicit e: c.WeakTypeTag[T]): c.Expr[T] = {
    import c.universe._
    c.Expr[T](q"${c.prefix}.macroW0[$e, upickle.old.Writer]")
  }
}
trait LowPriX{ this: Api =>
  implicit def macroR[T]: Reader[T] = macro Forwarder.applyR[T]
  implicit def macroW[T]: Writer[T] = macro Forwarder.applyW[T]
  def macroR0[T, M[_]]: Reader[T] = macro Macros.macroRImpl[T, M]
  def macroW0[T, M[_]]: Writer[T] = macro Macros.macroWImpl[T, M]
}
object Foo{
  type R[T] = old.Reader[T]
  type W[T] = old.Writer[T]
}