package upickle.implicits

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
  def applyRAll[T](c: scala.reflect.macros.blackbox.Context)
               (implicit e: c.WeakTypeTag[T]): c.Expr[T] = {
    import c.universe._
    dieIfNothing[T](c)("Reader")
    c.Expr[T](q"${c.prefix}.macroRAll0[$e, ${c.prefix}.Reader]")
  }
  def applyWAll[T](c: scala.reflect.macros.blackbox.Context)
               (implicit e: c.WeakTypeTag[T]): c.Expr[T] = {
    import c.universe._
    dieIfNothing[T](c)("Writer")
    c.Expr[T](q"${c.prefix}.macroWAll0[$e, ${c.prefix}.Writer]")
  }

  def applyRW[T](c: scala.reflect.macros.blackbox.Context)
                (implicit e: c.WeakTypeTag[T]): c.Expr[T] = {
    import c.universe._
    dieIfNothing[T](c)("Writer")
    c.Expr[T](q"${c.prefix}.ReadWriter.join(${c.prefix}.macroR, ${c.prefix}.macroW)")
  }

  def applyRWAll[T](c: scala.reflect.macros.blackbox.Context)
                (implicit e: c.WeakTypeTag[T]): c.Expr[T] = {
    import c.universe._
    dieIfNothing[T](c)("Writer")
    c.Expr[T](q"${c.prefix}.ReadWriter.join(${c.prefix}.macroRAll, ${c.prefix}.macroWAll)")
  }

}
trait MacroImplicits extends MacrosCommon { this: upickle.core.Types =>
  def macroR[T]: Reader[T] = macro MacroImplicits.applyR[T]
  def macroW[T]: Writer[T] = macro MacroImplicits.applyW[T]
  def macroRW[T]: ReadWriter[T] = macro MacroImplicits.applyRW[ReadWriter[T]]

  def macroRAll[T]: Reader[T] = macro MacroImplicits.applyRAll[T]
  def macroWAll[T]: Writer[T] = macro MacroImplicits.applyWAll[T]
  def macroRWAll[T]: ReadWriter[T] = macro MacroImplicits.applyRWAll[ReadWriter[T]]

  def macroR0[T, M[_]]: Reader[T] = macro internal.Macros.macroRImpl[T, M]
  def macroW0[T, M[_]]: Writer[T] = macro internal.Macros.macroWImpl[T, M]

  def macroRAll0[T, M[_]]: Reader[T] = macro internal.Macros.macroRAllImpl[T, M]
  def macroWAll0[T, M[_]]: Writer[T] = macro internal.Macros.macroWAllImpl[T, M]
}

