package upickle
import language.experimental.macros

/**
  * Stupid hacks to work around scalac not forwarding macro type params properly
  */
object Forwarder{
  def dieIfNothing[T: c.WeakTypeTag]
  (c: scala.reflect.macros.blackbox.Context)
  (name: String) = {
    if (c.weakTypeOf[T] =:= c.weakTypeOf[Nothing]) {
      c.abort(
        c.enclosingPosition,
        s"uPickle is trying to infer a $name[Nothing]. That probably means you messed up"
      )
    }
  }
  def applyR[T](c: scala.reflect.macros.blackbox.Context)
               (implicit e: c.WeakTypeTag[T]): c.Expr[T] = {
    import c.universe._
    dieIfNothing[T](c)("Reader")
    c.Expr[T](q"${c.prefix}.macroR0[$e, ${c.prefix}.Reader]")
  }
  def applyW[T](c: scala.reflect.macros.blackbox.Context)
               (implicit e: c.WeakTypeTag[T]): c.Expr[T] = {
    import c.universe._
    dieIfNothing[T](c)("Writer")
    c.Expr[T](q"${c.prefix}.macroW0[$e, ${c.prefix}.Writer]")
  }

  def applyRW[T](c: scala.reflect.macros.blackbox.Context)
                (implicit e: c.WeakTypeTag[T]): c.Expr[T] = {
    import c.universe._
    dieIfNothing[T](c)("Writer")
    c.Expr[T](q"${c.prefix}.macroRW0(${c.prefix}.macroR, ${c.prefix}.macroW)")
  }

}
trait LowPriImplicits{ this: Types =>
  implicit def macroSingletonR[T <: Singleton]: Reader[T] = macro Forwarder.applyR[T]
  implicit def macroSingletonW[T <: Singleton]: Writer[T] = macro Forwarder.applyW[T]
  implicit def macroSingletonRW[T <: Singleton]: ReadWriter[T] = macro Forwarder.applyRW[T]
  def macroR[T]: Reader[T] = macro Forwarder.applyR[T]
  def macroW[T]: Writer[T] = macro Forwarder.applyW[T]
  def macroRW[T]: Reader[T] with Writer[T] = macro Forwarder.applyRW[Reader[T] with Writer[T]]
  def macroRW0[T](r: Reader[T], w: Writer[T]): ReadWriter[T] = {
    (r, w) match{
      case (x: TaggedReader[T], y: TaggedWriter[T]) => joinTagged[T](x, y)
      case (x, y) => ReadWriter.join[T](x, y)
    }
  }
  def macroR0[T, M[_]]: Reader[T] = macro Macros.macroRImpl[T, M]
  def macroW0[T, M[_]]: Writer[T] = macro Macros.macroWImpl[T, M]
}

