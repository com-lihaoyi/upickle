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

  def visitInt8(i: Byte, index: Int): J = visitNumRaw(i, index)
  def visitInt16(i: Short, index: Int): J = visitNumRaw(i, index)
  def visitInt32(i: Int, index: Int): J = visitNumRaw(i, index)
  def visitUInt8(i: Byte, index: Int): J = visitNumRaw(i & 0xff, index)
  def visitUInt16(i: Short, index: Int): J = visitNumRaw(i & 0xffff, index)
  def visitUInt32(i: Int, index: Int): J = visitNumRawString(java.lang.Integer.toUnsignedString(i), index)

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

  def visitBin(bytes: Array[Byte], offset: Int, len: Int, index: Int): J = {
    visitString(upickle.core.Util.bytesToString(bytes.slice(offset, offset + len)), index)
  }

  def visitNum(s: CharSequence, decIndex: Int, expIndex: Int): J = visitNum(s, decIndex, expIndex, -1)

  def visitExt(tag: Byte, bytes: Array[Byte], offset: Int, len: Int, index: Int): J = {
    val arr = visitArray(-1, index)
    arr.visitValue(visitNumRaw(tag, index).asInstanceOf[T], -1)
    arr.visitValue(visitBin(bytes, offset, len, index).asInstanceOf[T], -1)
    arr.visitEnd(-1)
  }
}
