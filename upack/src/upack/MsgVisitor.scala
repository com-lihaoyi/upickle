package upack

import upickle.core.Visitor

/**
  * A [[Visitor]] specialized to work with msgpack types. Forwards the
  * not-msgpack-related methods to their msgpack equivalents.
  */
trait MsgVisitor[-T, +J] extends Visitor[T, J]{

  def visitFloat64String(s: String, index: Int) = this.visitFloat64(s.toDouble, index)

  def visitFloat64StringParts(s: CharSequence, decIndex: Int, expIndex: Int, index: Int) = {
    this.visitFloat64(s.toString.toDouble, index)
  }
}
