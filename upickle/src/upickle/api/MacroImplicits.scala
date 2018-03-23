package upickle
package api

import upickle.json.{AbortJsonProcessingException, ObjArrVisitor, Visitor}

import language.experimental.macros

/**
  * Stupid hacks to work around scalac not forwarding macro type params properly
  */
object MacroImplicits{
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
trait MacroImplicits{ this: core.Types =>
  implicit def macroSingletonR[T <: Singleton]: Reader[T] = macro MacroImplicits.applyR[T]
  implicit def macroSingletonW[T <: Singleton]: Writer[T] = macro MacroImplicits.applyW[T]
  implicit def macroSingletonRW[T <: Singleton]: ReadWriter[T] = macro MacroImplicits.applyRW[T]
  def macroR[T]: Reader[T] = macro MacroImplicits.applyR[T]
  def macroW[T]: Writer[T] = macro MacroImplicits.applyW[T]
  def macroRW[T]: Reader[T] with Writer[T] = macro MacroImplicits.applyRW[Reader[T] with Writer[T]]
  def macroRW0[T](r: Reader[T], w: Writer[T]): ReadWriter[T] = (r, w) match{
    case (r1: TaggedReader[T], w1: TaggedWriter[T]) =>
      new TaggedReadWriter[T] {
        def findReader(s: String) = r1.findReader(s)
        def findWriter(v: Any) = w1.findWriter(v)
      }

    case _ =>
      new BaseReader.Delegate[Any, T] with Writer[T]{
        override def narrow[K <: T] = this.asInstanceOf[ReadWriter[K]]
        def delegatedReader = r
        def write0[V](out: Visitor[_, V], v: T) = w.write(out, v)
      }
  }
  def macroR0[T, M[_]]: Reader[T] = macro internal.Macros.macroRImpl[T, M]
  def macroW0[T, M[_]]: Writer[T] = macro internal.Macros.macroWImpl[T, M]
}

