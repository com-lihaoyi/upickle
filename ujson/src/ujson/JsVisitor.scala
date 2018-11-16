package ujson

import upickle.core.{ArrVisitor, ObjVisitor, Visitor}

/**
  * A [[Visitor]] specialized to work with JSON types. Forwards the
  * not-JSON-related methods to their JSON equivalents.
  */
trait JsVisitor[-T, +J] extends Visitor[T, J]{

  def visitNumRaw(d: Double, index: Int): J = {
    val i = d.toLong
    if(i == d) visitNum(i.toString, -1, -1, index)
    else visitNumRawString(d.toString, index)

  }

  def visitNum32(d: Float, index: Int): J = visitNumRaw(d, index)

  def visitInt32(i: Int, index: Int): J = visitNumRaw(i, index)
  def visitUInt32(i: Int, index: Int): J = visitNumRaw(i & 0x00000000ffffffffL, index)

  def visitInt64(i: Long, index: Int): J = {
    if (math.abs(i) > math.pow(2, 53) || i == -9223372036854775808L) visitString(i.toString, index)
    else visitNumRaw(i, index)
  }
  def visitUInt64(i: Long, index: Int): J = {
    if (i > math.pow(2, 53) || i < 0) visitString(java.lang.Long.toUnsignedString(i), index)
    else visitNumRaw(i, index)
  }

  def visitNumRawString(s: String, index: Int): J = {
    visitNum(s, s.indexOf('.'), s.indexOf('E') match{case -1 => s.indexOf('e') case n => n}, -1)
  }

  def visitBin(bytes: Array[Byte], offset: Int, len: Int, index: Int): J = ???

  def visitNum(s: CharSequence, decIndex: Int, expIndex: Int): J = visitNum(s, decIndex, expIndex, -1)
}
