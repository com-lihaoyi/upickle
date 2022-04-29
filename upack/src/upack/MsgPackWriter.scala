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
      byteBuilder.ensureLength(3)
      byteBuilder.appendUnsafe(MPK.Array16.toByte)
      writeUInt16(length)
    }else {
      byteBuilder.ensureLength(5)
      byteBuilder.appendUnsafe(MPK.Array32.toByte)
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

  override def visitObject(length: Int, jsonableKeys: Boolean, index: Int) = new ObjVisitor[T, T] {
    require(length != -1, "Length of upack object must be known up front")
    depth += 1
    if (length <= 15){
      byteBuilder.append(MPK.FixMapMask | length)
    }else if (length <= 65535){
      byteBuilder.ensureLength(3)
      byteBuilder.appendUnsafe(MPK.Map16.toByte)
      writeUInt16(length)
    }else {
      byteBuilder.ensureLength(5)
      byteBuilder.appendUnsafe(MPK.Map32.toByte)
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
    byteBuilder.ensureLength(9)
    byteBuilder.append(MPK.Float64)
    writeUInt64(java.lang.Double.doubleToLongBits(d))
    flushElemBuilder()
    out
  }
  override def visitFloat32(d: Float, index: Int) = {
    byteBuilder.ensureLength(5)
    byteBuilder.append(MPK.Float32)
    writeUInt32(java.lang.Float.floatToIntBits(d))
    flushElemBuilder()
    out
  }
  override def visitInt32(i: Int, index: Int) = {
    if (i >= 0){
      if (i <= 127) byteBuilder.append(i)
      else if (i <= 255){
        byteBuilder.ensureLength(2)
        byteBuilder.appendUnsafe(MPK.UInt8.toByte)
        byteBuilder.appendUnsafe(i.toByte)
      } else if(i <= Short.MaxValue){
        byteBuilder.ensureLength(3)
        byteBuilder.appendUnsafe(MPK.Int16.toByte)
        writeUInt16(i)
      } else if (i <= 0xffff){
        byteBuilder.ensureLength(5)
        byteBuilder.appendUnsafe(MPK.UInt16.toByte)
        writeUInt16(i)
      } else{
        byteBuilder.ensureLength(5)
        byteBuilder.appendUnsafe(MPK.Int32.toByte)
        writeUInt32(i)
      }
    }else{
      if (i >= -32) byteBuilder.append(i | 0xe0)
      else if(i >= -128){
        byteBuilder.ensureLength(2)
        byteBuilder.appendUnsafe(MPK.Int8.toByte)
        byteBuilder.appendUnsafe(i.toByte)
      }else if (i >= Short.MinValue) {
        byteBuilder.ensureLength(3)
        byteBuilder.appendUnsafe(MPK.Int16.toByte)
        writeUInt16(i)
      } else{
        byteBuilder.ensureLength(5)
        byteBuilder.appendUnsafe(MPK.Int32.toByte)
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
      byteBuilder.ensureLength(5)
      byteBuilder.appendUnsafe(MPK.UInt32.toByte)
      writeUInt32(i.toInt)
    }else{
      byteBuilder.ensureLength(9)
      byteBuilder.appendUnsafe(MPK.Int64.toByte)
      writeUInt64(i)
    }
    flushElemBuilder()
    out
  }

  override def visitUInt64(i: Long, index: Int) = {
    if (i >= 0) visitInt64(i, index)
    else{
      byteBuilder.ensureLength(9)
      byteBuilder.appendUnsafe(MPK.UInt64.toByte)
      writeUInt64(i)
    }
    flushElemBuilder()
    out
  }

  override def visitString(s: CharSequence, index: Int) = {
    val strBytes = s.toString.getBytes(java.nio.charset.StandardCharsets.UTF_8)
    val length = strBytes.length
    if (length <= 31){
      byteBuilder.ensureLength(1 + length)
      byteBuilder.appendUnsafe((MPK.FixStrMask | length).toByte)
    } else if (length <= 255){
      byteBuilder.ensureLength(2 + length)
      byteBuilder.appendUnsafe(MPK.Str8.toByte)
      writeUInt8(length)
    }else if (length <= 65535){
      byteBuilder.ensureLength(3 + length)
      byteBuilder.appendUnsafe(MPK.Str16.toByte)
      writeUInt16(length)
    }else {
      byteBuilder.ensureLength(5 + length)
      byteBuilder.appendUnsafe(MPK.Str32.toByte)
      writeUInt32(length)
    }

    byteBuilder.appendAll(strBytes, length)
    flushElemBuilder()
    out
  }
  override def visitBinary(bytes: Array[Byte], offset: Int, len: Int, index: Int) = {
    if (len <= 255) {
      byteBuilder.ensureLength(2)
      byteBuilder.appendUnsafe(MPK.Bin8.toByte)
      writeUInt8(len)
    } else if (len <= 65535) {
      byteBuilder.ensureLength(3)
      byteBuilder.appendUnsafe(MPK.Bin16.toByte)
      writeUInt16(len)
    } else {
      byteBuilder.ensureLength(5)
      byteBuilder.appendUnsafe(MPK.Bin32.toByte)
      writeUInt32(len)
    }

    byteBuilder.appendAll(bytes, offset, len)
    flushElemBuilder()
    out
  }
  def writeUInt8(i: Int) = byteBuilder.appendUnsafe(i.toByte)
  def writeUInt16(i: Int) = {
    byteBuilder.appendUnsafe(((i >> 8) & 0xff).toByte)
    byteBuilder.appendUnsafe(((i >> 0) & 0xff).toByte)
  }
  def writeUInt32(i: Int) = {
    byteBuilder.appendUnsafe(((i >> 24) & 0xff).toByte)
    byteBuilder.appendUnsafe(((i >> 16) & 0xff).toByte)
    byteBuilder.appendUnsafe(((i >> 8) & 0xff).toByte)
    byteBuilder.appendUnsafe(((i >> 0) & 0xff).toByte)
  }
  def writeUInt64(i: Long) = {
    byteBuilder.appendUnsafe(((i >> 56) & 0xff).toByte )
    byteBuilder.appendUnsafe(((i >> 48) & 0xff).toByte )
    byteBuilder.appendUnsafe(((i >> 40) & 0xff).toByte )
    byteBuilder.appendUnsafe(((i >> 32) & 0xff).toByte )
    byteBuilder.appendUnsafe(((i >> 24) & 0xff).toByte )
    byteBuilder.appendUnsafe(((i >> 16) & 0xff).toByte )
    byteBuilder.appendUnsafe(((i >> 8) & 0xff).toByte )
    byteBuilder.appendUnsafe(((i >> 0) & 0xff).toByte )
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
          byteBuilder.ensureLength(2)
          byteBuilder.appendUnsafe(MPK.Ext8.toByte)
          writeUInt8(len)
        }else if (len <= 65535){
          byteBuilder.ensureLength(3)
          byteBuilder.appendUnsafe(MPK.Ext16.toByte)
          writeUInt16(len)
        }else{
          byteBuilder.ensureLength(5)
          writeUInt32(len)
          byteBuilder.appendUnsafe(MPK.Ext32.toByte)
        }
    }
    byteBuilder.append(tag)
    byteBuilder.appendAll(bytes, offset, len)
    flushElemBuilder()
    out
  }

  def visitChar(s: Char, index: Int) = {
    byteBuilder.ensureLength(3)
    byteBuilder.appendUnsafe(MPK.UInt16.toByte)
    writeUInt16(s)
    flushElemBuilder()
    out
  }
}
