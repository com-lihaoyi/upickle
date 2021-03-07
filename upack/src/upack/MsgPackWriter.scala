package upack
import java.io.ByteArrayOutputStream

import upack.{MsgPackKeys => MPK}
import upickle.core.{ArrVisitor, ObjVisitor, Visitor}
class MsgPackWriter[T <: java.io.OutputStream](out: T = new ByteArrayOutputStream())
    extends MsgVisitor[T, T] {
  private[this] val byteBuilder = new upickle.core.ByteBuilder()
  def flushElemBuilder() = {
    byteBuilder.writeOutToIfLongerThan(out, if (depth == 0) 0 else 1000)
  }
  private[this] var depth = 0
  override def visitArray(length: Int, index: Int) = new ArrVisitor[T, T] {
    require(length != -1, "Length of upack array must be known up front")
    depth += 1
    if (length <= 15){
      byteBuilder.append(MPK.FixArrMask | length)
    }else if (length <= 65535){
      byteBuilder.append(MPK.Array16)
      writeUInt16(length)
    }else {
      byteBuilder.append(MPK.Array32)
      writeUInt32(length)
    }
    def subVisitor = MsgPackWriter.this
    def visitValue(v: T, index: Int): Unit = () // do nothing
    def visitEnd(index: Int) = {
      depth -= 1
      flushElemBuilder()
      out
    }
  }

  override def visitObject(length: Int, index: Int) = new ObjVisitor[T, T] {
    require(length != -1, "Length of upack object must be known up front")
    depth += 1
    if (length <= 15){
      byteBuilder.append(MPK.FixMapMask | length)
    }else if (length <= 65535){
      byteBuilder.append(MPK.Map16)
      writeUInt16(length)
    }else {
      byteBuilder.append(MPK.Map32)
      writeUInt32(length)
    }
    def subVisitor = MsgPackWriter.this
    def visitKey(index: Int)= MsgPackWriter.this
    def visitKeyValue(s: Any): Unit = () // do nothing
    def visitValue(v: T, index: Int): Unit = () // do nothing
    def visitEnd(index: Int) = {
      depth -= 1
      flushElemBuilder()
      out
    }
  }


  override def visitNull(index: Int) = {
    byteBuilder.append(MPK.Nil)
    flushElemBuilder()
    out
  }

  override def visitFalse(index: Int) = {
    byteBuilder.append(MPK.False)
    flushElemBuilder()
    out
  }

  override def visitTrue(index: Int) = {
    byteBuilder.append(MPK.True)
    flushElemBuilder()
    out
  }

  override def visitFloat64StringParts(s: CharSequence, decIndex: Int, expIndex: Int, index: Int) = {
    visitFloat64(s.toString.toDouble, index)
  }

  override def visitFloat64(d: Double, index: Int) = {
    byteBuilder.append(MPK.Float64)
    writeUInt64(java.lang.Double.doubleToLongBits(d))
    flushElemBuilder()
    out
  }
  override def visitFloat32(d: Float, index: Int) = {
    byteBuilder.append(MPK.Float32)
    writeUInt32(java.lang.Float.floatToIntBits(d))
    flushElemBuilder()
    out
  }
  override def visitInt32(i: Int, index: Int) = {
    if (i >= 0){
      if (i <= 127) byteBuilder.append(i)
      else if (i <= 255){
        byteBuilder.append(MPK.UInt8)
        byteBuilder.append(i)
      } else if(i <= Short.MaxValue){
        byteBuilder.append(MPK.Int16)
        writeUInt16(i)
      } else if (i <= 0xffff){
        byteBuilder.append(MPK.UInt16)
        writeUInt16(i)
      } else{
        byteBuilder.append(MPK.Int32)
        writeUInt32(i)
      }
    }else{
      if (i >= -32) byteBuilder.append(i | 0xe0)
      else if(i >= -128){
        byteBuilder.append(MPK.Int8)
        byteBuilder.append(i)
      }else if (i >= Short.MinValue) {
        byteBuilder.append(MPK.Int16)
        writeUInt16(i)
      } else{
        byteBuilder.append(MPK.Int32)
        writeUInt32(i)
      }
    }
    flushElemBuilder()
    out
  }

  override def visitInt64(i: Long, index: Int) = {
    if (i >= Int.MinValue && i <= Int.MaxValue){
      visitInt32(i.toInt, index)
    }else if (i >= 0 && i <= 0xffffffffL){
      byteBuilder.append(MPK.UInt32)
      writeUInt32(i.toInt)
    }else{
      byteBuilder.append(MPK.Int64)
      writeUInt64(i)
    }
    flushElemBuilder()
    out
  }

  override def visitUInt64(i: Long, index: Int) = {
    if (i >= 0) visitInt64(i, index)
    else{
      byteBuilder.append(MPK.UInt64)
      writeUInt64(i)
    }
    flushElemBuilder()
    out
  }

  val stringTempByteBuilder = new upickle.core.ByteBuilder()
  val stringTempCharBuilder = new upickle.core.CharBuilder()
  override def visitString(s: CharSequence, index: Int) = {
    stringTempByteBuilder.reset()
    upickle.core.RenderUtils.encodeCharSequenceToBytes(
      stringTempCharBuilder,
      stringTempByteBuilder,
      s
    )
    val length = stringTempByteBuilder.getLength
    if (length <= 31){
      byteBuilder.append(MPK.FixStrMask | length)
    } else if (length <= 255){
      byteBuilder.append(MPK.Str8)
      writeUInt8(length)
    }else if (length <= 65535){
      byteBuilder.append(MPK.Str16)
      writeUInt16(length)
    }else {
      byteBuilder.append(MPK.Str32)
      writeUInt32(length)
    }

    byteBuilder.appendAll(stringTempByteBuilder)
    flushElemBuilder()
    out
  }
  override def visitBinary(bytes: Array[Byte], offset: Int, len: Int, index: Int) = {
    if (len <= 255) {
      byteBuilder.append(MPK.Bin8)
      writeUInt8(len)
    } else if (len <= 65535) {
      byteBuilder.append(MPK.Bin16)
      writeUInt16(len)
    } else {
      byteBuilder.append(MPK.Bin32)
      writeUInt32(len)
    }

    byteBuilder.appendAll(bytes, offset, len)
    flushElemBuilder()
    out
  }
  def writeUInt8(i: Int) = byteBuilder.append(i)
  def writeUInt16(i: Int) = {
    byteBuilder.append((i >> 8) & 0xff)
    byteBuilder.append((i >> 0) & 0xff)
  }
  def writeUInt32(i: Int) = {
    byteBuilder.append((i >> 24) & 0xff)
    byteBuilder.append((i >> 16) & 0xff)
    byteBuilder.append((i >> 8) & 0xff)
    byteBuilder.append((i >> 0) & 0xff)
  }
  def writeUInt64(i: Long) = {
    byteBuilder.append(((i >> 56) & 0xff).toInt)
    byteBuilder.append(((i >> 48) & 0xff).toInt)
    byteBuilder.append(((i >> 40) & 0xff).toInt)
    byteBuilder.append(((i >> 32) & 0xff).toInt)
    byteBuilder.append(((i >> 24) & 0xff).toInt)
    byteBuilder.append(((i >> 16) & 0xff).toInt)
    byteBuilder.append(((i >> 8) & 0xff).toInt)
    byteBuilder.append(((i >> 0) & 0xff).toInt)
  }

  def visitExt(tag: Byte, bytes: Array[Byte], offset: Int, len: Int, index: Int) = {
    len match{
      case 1 => byteBuilder.append(MPK.FixExt1)
      case 2 => byteBuilder.append(MPK.FixExt2)
      case 4 => byteBuilder.append(MPK.FixExt4)
      case 8 => byteBuilder.append(MPK.FixExt8)
      case 16 => byteBuilder.append(MPK.FixExt16)
      case _ =>
        if (len <= 255){
          byteBuilder.append(MPK.Ext8)
          writeUInt8(len)
        }else if (len <= 65535){
          byteBuilder.append(MPK.Ext16)
          writeUInt16(len)
        }else{
          writeUInt32(len)
          byteBuilder.append(MPK.Ext32)
        }
    }
    byteBuilder.append(tag)
    byteBuilder.appendAll(bytes, offset, len)
    flushElemBuilder()
    out
  }

  def visitChar(s: Char, index: Int) = {
    byteBuilder.append(MPK.UInt16)
    writeUInt16(s)
    flushElemBuilder()
    out
  }
}
