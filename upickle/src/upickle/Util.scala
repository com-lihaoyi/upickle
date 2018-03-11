package upickle

import jawn.RawFContext

object Util {
  def mapContext[T, V, Z](in: RawFContext[T, V])(f: V => Z) = new RawFContext[T, Z] {
    def facade = in.facade

    def visitKey(s: CharSequence, index: Int): Unit = in.visitKey(s, index)

    def add(v: T, index: Int): Unit = in.add(v, index)

    def finish(index: Int) = f(in.finish(index))

    def isObj = in.isObj
  }
}
