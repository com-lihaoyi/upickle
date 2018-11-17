package upickle.core

trait CustomVisitor[-T, +V] extends Visitor[T, V] {
  def expectedMsg: String
  def visitNull(index: Int): V = null.asInstanceOf[V]
  def visitTrue(index: Int): V =  throw new Abort(expectedMsg + " got boolean")
  def visitFalse(index: Int): V = throw new Abort(expectedMsg + " got boolean")

  def visitString(s: CharSequence, index: Int): V = {
    throw new Abort(expectedMsg + " got string")
  }
  def visitFloat64StringParts(s: CharSequence, decIndex: Int, expIndex: Int, index: Int): V = {
    throw new Abort(expectedMsg + " got number")
  }

  def visitObject(length: Int, index: Int): ObjVisitor[T, V] = {
    throw new Abort(expectedMsg + " got dictionary")
  }
  def visitArray(length: Int, index: Int): ArrVisitor[T, V] = {
    throw new Abort(expectedMsg + " got sequence")
  }

  def visitFloat64(d: Double, index: Int): V = throw new Abort(expectedMsg + " got float64")

  def visitFloat32(d: Float, index: Int): V = throw new Abort(expectedMsg + " got float32")

  def visitInt32(i: Int, index: Int): V = throw new Abort(expectedMsg + " got int32")

  def visitInt64(i: Long, index: Int): V = throw new Abort(expectedMsg + " got int64")

  def visitUInt64(i: Long, index: Int): V = throw new Abort(expectedMsg + " got uint64")

  def visitFloat64String(s: String, index: Int): V = throw new Abort(expectedMsg + " got float64 string")

  def visitChar(s: Char, index: Int): V = throw new Abort(expectedMsg + " got char")

  def visitBin(bytes: Array[Byte], offset: Int, len: Int, index: Int): V = throw new Abort(expectedMsg + " got binary")

  def visitExt(tag: Byte, bytes: Array[Byte], offset: Int, len: Int, index: Int): V = throw new Abort(expectedMsg + " got ext")
}
