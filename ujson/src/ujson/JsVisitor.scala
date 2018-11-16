package ujson

import upickle.core.{ArrVisitor, ObjVisitor, Visitor}

/**
  * A [[Visitor]] specialized to work with JSON types. Forwards the
  * not-JSON-related methods to their JSON equivalents.
  */
trait JsVisitor[-T, +J] extends Visitor[T, J]{

  def visitNumRaw(d: Double, index: Int): J = {
    val i = d.toInt
    if(i == d) visitNum(i.toString, -1, -1, index)
    else visitNumRawString(d.toString, index)
  }

  def visitNum32(d: Float, index: Int): J = visitNumRaw(d, index)

  def visitInt32(i: Int, index: Int): J = visitNumRaw(i, index)

  def visitInt64(i: Long, index: Int): J = visitNumRaw(i, index)

  def visitNumRawString(s: String, index: Int): J = {
    visitNum(s, s.indexOf('.'), s.indexOf('E') match{case -1 => s.indexOf('e') case n => n}, -1)
  }

  def visitBin(bytes: Array[Byte], offset: Int, len: Int, index: Int): J = ???

  def visitNum(s: CharSequence, decIndex: Int, expIndex: Int): J = visitNum(s, decIndex, expIndex, -1)
}
