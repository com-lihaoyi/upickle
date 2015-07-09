package upickle


import language.experimental.macros
import scala.reflect.ClassTag

/**
* Created by haoyi on 7/4/15.
*/
abstract class Api extends Types with Implicits with Generated with LowPriX{
  protected[this] def validate[T](name: String)(pf: PartialFunction[Js.Value, T]) = Internal.validate(name)(pf)

  type key = derive.key
  def annotate[V: ClassTag](rw: Reader[V], n: String): Reader[V]
  def annotate[V: ClassTag](rw: Writer[V], n: String): Writer[V]
}
object legacy extends Api{
  def annotate[V: ClassTag](rw: Reader[V], n: String) = Reader[V]{
    case Js.Arr(Js.Str(`n`), x) => rw.read(x)
  }

  def annotate[V: ClassTag](rw: Writer[V], n: String) = Writer[V]{
    case x: V => Js.Arr(Js.Str(n), rw.write(x))
  }
}
class AttributeTagged(tagName: String) extends Api{
  def annotate[V: ClassTag](rw: Reader[V], n: String) = Reader[V]{
    case Js.Obj(x@_*) if x.contains((tagName, Js.Str(n))) =>
    rw.read(Js.Obj(x.filter(_._1 != tagName):_*))

  }

  def annotate[V: ClassTag](rw: Writer[V], n: String) = Writer[V]{ case x: V =>
    Js.Obj((tagName, Js.Str(n)) +: rw.write(x).asInstanceOf[Js.Obj].value:_*)
  }
}
object default extends AttributeTagged("$type")
object Forwarder{
  def applyR[T](c: derive.ScalaVersionStubs.Context)
              (implicit e: c.WeakTypeTag[T]): c.Expr[T] = {
    import c.universe._
    c.Expr[T](q"${c.prefix}.macroR0[$e, ${c.prefix}.Reader]")
  }
  def applyW[T](c: derive.ScalaVersionStubs.Context)
              (implicit e: c.WeakTypeTag[T]): c.Expr[T] = {
    import c.universe._
    c.Expr[T](q"${c.prefix}.macroW0[$e, ${c.prefix}.Writer]")
  }
}
trait LowPriX{ this: Api =>
  implicit def macroR[T]: Reader[T] = macro Forwarder.applyR[T]
  implicit def macroW[T]: Writer[T] = macro Forwarder.applyW[T]
  def macroR0[T, M[_]]: Reader[T] = macro Macros.macroRImpl[T, M]
  def macroW0[T, M[_]]: Writer[T] = macro Macros.macroWImpl[T, M]
}