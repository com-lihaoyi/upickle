package upack
import upack.{MsgPackKeys => MPK}
import upickle.core.{ArrVisitor, ObjVisitor, Visitor}
class MsgPackWriter[T <: java.io.OutputStream](out: T) extends Visitor[T, T] {
  override def visitArray(length: Int, index: Int) = new ArrVisitor[T, T] {
    if (length <= 15){
      out.write(MPK.FixArrMask | length)
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
    if (length <= 15){
      out.write(MPK.FixMapMask | length)
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

  override def visitFloat64StringParts(s: CharSequence, decIndex: Int, expIndex: Int, index: Int) = {
    visitFloat64(s.toString.toDouble, index)
  }

  override def visitFloat64(d: Double, index: Int) = {
    out.write(MPK.Float64)
    writeUInt64(java.lang.Double.doubleToLongBits(d))
    out
  }
  override def visitFloat32(d: Float, index: Int) = {
    out.write(MPK.Float32)
    writeUInt32(java.lang.Float.floatToIntBits(d))
    out
  }
  override def visitInt8(i: Byte, index: Int) = {

    if (i >= 0){
      out.write(i)
    }else if (i >= -32) {
      out.write(i | 0xe0)
    }else{
      out.write(MPK.Int8)
      out.write(i)
    }
    out
  }
  override def visitUInt8(i: Byte, index: Int) = {
    out.write(MPK.UInt8)
    writeUInt8(i)
    out
  }
  override def visitInt16(i: Short, index: Int) = {
    out.write(MPK.Int16)
    writeUInt16(i)
    out
  }
  override def visitUInt16(i: Short, index: Int) = {
    out.write(MPK.UInt16)
    writeUInt16(i)
    out
  }
  override def visitInt32(i: Int, index: Int) = {
    out.write(MPK.Int32)
    writeUInt32(i)
    out
  }
  override def visitUInt32(i: Int, index: Int) = {
    out.write(MPK.UInt32)
    writeUInt32(i)
    out
  }

  override def visitInt64(i: Long, index: Int) = {
    out.write(MPK.Int64)
    writeUInt64(i)
    out
  }
  override def visitUInt64(i: Long, index: Int) = {
    out.write(MPK.UInt64)
    writeUInt64(i)
    out
  }
  override def visitString(s: CharSequence, index: Int) = {
    val bytes = s.toString.getBytes("UTF-8")
    val length = bytes.length
    if (length <= 31){
      out.write(MPK.FixStrMask | length)
    } else if (length <= 255){
      out.write(MPK.Str8)
      writeUInt8(length)
    }else if (length <= 65535){
      out.write(MPK.Str16)
      writeUInt16(length)
    }else {
      out.write(MPK.Str32)
      writeUInt32(length)
    }

    out.write(bytes, 0, length)
    out
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

  def visitFloat64String(s: String, index: Int) = ???

  def visitFloat64StringParts(s: CharSequence, decIndex: Int, expIndex: Int) = ???

  def visitExt(tag: Byte, bytes: Array[Byte], offset: Int, len: Int, index: Int) = ???
}
