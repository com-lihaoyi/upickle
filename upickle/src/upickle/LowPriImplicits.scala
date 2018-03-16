package upickle
import upickle.jawn.Facade

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
  def macroRW0[T](r: Reader[T], w: Writer[T]): ReadWriter[T] = (r, w) match{
    case (r1: TaggedReader[T], w1: TaggedWriter[T]) =>
      def rec(r2: TaggedReader[_], w2: TaggedWriter[_]): TaggedReadWriter[_] = {
        (r2, w2) match{
          case (TaggedReadWriter.Node(rs@_*), TaggedReadWriter.Node(ws@_*)) =>
            TaggedReadWriter.Node(rs.zip(ws).map(rec _ tupled):_*)
          case (TaggedReader.Node(rs@_*), TaggedWriter.Node(ws@_*)) =>
            TaggedReadWriter.Node(rs.zip(ws).map(rec _ tupled):_*)
          case (TaggedReader.Leaf(t, r3), TaggedWriter.Leaf(c, t2, w3)) =>
            TaggedReadWriter.Leaf(c, t,
              new BaseReader.Delegate[Any, T] with Writer[T]{
                def delegatedReader = r3.asInstanceOf[Reader[T]]
                def write[V](out: Facade[V], v: T) = w3.asInstanceOf[Writer[T]].write(out, v)
              }
            )
          case (TaggedReadWriter.Leaf(c1, t, r3), TaggedReadWriter.Leaf(c, t2, w3)) =>
            TaggedReadWriter.Leaf(c, t,
              new BaseReader.Delegate[Any, T] with Writer[T]{
                def delegatedReader = r3.asInstanceOf[Reader[T]]
                def write[V](out: Facade[V], v: T) = w3.asInstanceOf[Writer[T]].write(out, v)
              }
            )
        }
      }
      rec(r1, w1).asInstanceOf[TaggedReadWriter[T]]
    case _ =>
      new BaseReader.Delegate[Any, T] with Writer[T]{
        def delegatedReader = r
        def write[V](out: Facade[V], v: T) = w.write(out, v)
      }
  }
  def macroR0[T, M[_]]: Reader[T] = macro Macros.macroRImpl[T, M]
  def macroW0[T, M[_]]: Writer[T] = macro Macros.macroWImpl[T, M]
}

