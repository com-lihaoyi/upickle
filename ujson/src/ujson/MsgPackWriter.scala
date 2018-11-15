package ujson
import ujson.{MsgPackKeys => MPK}

class MsgPackWriter[T <: java.io.OutputStream](out: T) extends Visitor[T, T] {
  override def visitArray(length: Int, index: Int) = new ArrVisitor[T, T] {
    if (length <= 255){
      out.write(MPK.FixArray | length)
    }else if (length <= 65535){
      out.write(MPK.Array16)
      writeUInt16(length)
    }else {
      out.write(MPK.Array32)
      writeUInt32(length)
    }
    def subVisitor = MsgPackWriter.this
    def visitValue(v: T, index: Int): Unit = () // do nothing
    def visitEnd(index: Int) = out // do nothing
  }
  override def visitObject(length: Int, index: Int) = new ObjVisitor[T, T] {
    if (length <= 255){
      out.write(MPK.FixMap | length)
    }else if (length <= 65535){
      out.write(MPK.Map16)
      writeUInt16(length)
    }else {
      out.write(MPK.Map32)
      writeUInt32(length)
    }
    def subVisitor = MsgPackWriter.this
    def visitKey(s: CharSequence, index: Int): Unit = visitString(s, index)
    def visitValue(v: T, index: Int): Unit = () // do nothing
    def visitEnd(index: Int) = out // do nothing
  }


  override def visitNull(index: Int) = {
    out.write(MPK.Nil)
    out
  }

  override def visitFalse(index: Int) = {
    out.write(MPK.False)
    out
  }

  override def visitTrue(index: Int) = {
    out.write(MPK.True)
    out
  }

  override def visitNum(s: CharSequence, decIndex: Int, expIndex: Int, index: Int) = {
    visitNumRaw(s.toString.toDouble, index)
  }

  override def visitNumRaw(d: Double, index: Int) = {
    writeUInt64(java.lang.Double.doubleToLongBits(d))
    out
  }
  override def visitNum32(d: Float, index: Int) = {
    writeUInt32(java.lang.Float.floatToIntBits(d))
    out
  }
  override def visitInt32(i: Int, index: Int) = {
    writeUInt32(i)
    out
  }

  override def visitInt64(i: Long, index: Int) = {
    writeUInt64(i)
    out
  }
  override def visitString(s: CharSequence, index: Int) = {
    val bytes = s.toString.getBytes("UTF-8")
    visitBin(bytes, 0, bytes.length, index)
  }
  override def visitBin(bytes: Array[Byte], offset: Int, len: Int, index: Int) = {
    if (len <= 255) {
      out.write(MPK.Bin8)
      writeUInt8(len)
    } else if (len <= 65535) {
      out.write(MPK.Bin16)
      writeUInt16(len)
    } else {
      out.write(MPK.Bin32)
      writeUInt32(len)
    }

    out.write(bytes, offset, len)
    out
  }
  def writeUInt8(i: Int) = out.write(i)
  def writeUInt16(i: Int) = {
    out.write((i >> 8) & 0xff)
    out.write((i >> 0) & 0xff)
  }
  def writeUInt32(i: Int) = {
    out.write((i >> 24) & 0xff)
    out.write((i >> 16) & 0xff)
    out.write((i >> 8) & 0xff)
    out.write((i >> 0) & 0xff)
  }
  def writeUInt64(i: Long) = {
    out.write(((i >> 56) & 0xff).toInt)
    out.write(((i >> 48) & 0xff).toInt)
    out.write(((i >> 40) & 0xff).toInt)
    out.write(((i >> 32) & 0xff).toInt)
    out.write(((i >> 24) & 0xff).toInt)
    out.write(((i >> 16) & 0xff).toInt)
    out.write(((i >> 8) & 0xff).toInt)
    out.write(((i >> 0) & 0xff).toInt)
  }

}
