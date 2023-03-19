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
      val arr = byteBuilder.arr; val arrLength = byteBuilder.length
      arr(arrLength) = MPK.Array16.toByte
      writeUInt16(arr, arrLength, length)
      byteBuilder.length += 3
    }else {
      byteBuilder.ensureLength(5)
      val arr = byteBuilder.arr; val arrLength = byteBuilder.length
      arr(arrLength) = MPK.Array32.toByte
      writeUInt32(arr, arrLength, length)
      byteBuilder.length += 5
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
      val arr = byteBuilder.arr; val arrLength = byteBuilder.length
      arr(arrLength) = MPK.Map16.toByte
      writeUInt16(arr, arrLength, length)
      byteBuilder.length += 3
    }else {
      byteBuilder.ensureLength(5)
      val arr = byteBuilder.arr; val arrLength = byteBuilder.length
      arr(arrLength) = MPK.Map32.toByte
      writeUInt32(arr, arrLength, length)
      byteBuilder.length += 5
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
    val arr = byteBuilder.arr
    val length = byteBuilder.length
    arr(length) = MPK.Float64.toByte
    writeUInt64(arr, length, java.lang.Double.doubleToLongBits(d))
    byteBuilder.length = length + 9
    flushElemBuilder()
    out
  }
  override def visitFloat32(d: Float, index: Int) = {
    byteBuilder.ensureLength(5)
    val arr = byteBuilder.arr
    val length = byteBuilder.length
    arr(length) = MPK.Float32.toByte
    writeUInt32(arr, length, java.lang.Float.floatToIntBits(d))
    byteBuilder.length = length + 5
    flushElemBuilder()
    out
  }
  override def visitInt32(i: Int, index: Int) = {
    if (i >= 0){
      if (i <= 127) byteBuilder.append(i)
      else if (i <= 255){
        byteBuilder.ensureLength(2)
        val arr = byteBuilder.arr; val length = byteBuilder.length
        arr(length) = MPK.UInt8.toByte
        arr(length + 1) = i.toByte
        byteBuilder.length = length + 2
      } else if(i <= Short.MaxValue){
        byteBuilder.ensureLength(3)
        val arr = byteBuilder.arr; val length = byteBuilder.length
        arr(length) = MPK.Int16.toByte
        writeUInt16(arr, length, i)
        byteBuilder.length = length + 3
      } else if (i <= 0xffff){
        byteBuilder.ensureLength(3)
        val arr = byteBuilder.arr; val length = byteBuilder.length
        arr(length) = MPK.UInt16.toByte
        writeUInt16(arr, length, i)
        byteBuilder.length = length + 3
      } else{
        byteBuilder.ensureLength(5)
        val arr = byteBuilder.arr; val length = byteBuilder.length
        arr(length) = MPK.Int32.toByte
        writeUInt32(arr, length, i)
        byteBuilder.length = length + 5
      }
    }else{
      if (i >= -32) byteBuilder.append(i | 0xe0)
      else if(i >= -128){
        byteBuilder.ensureLength(2)
        val arr = byteBuilder.arr; val length = byteBuilder.length
        arr(length) = MPK.Int8.toByte
        arr(length + 1) = i.toByte
        byteBuilder.length = length + 2
      }else if (i >= Short.MinValue) {
        byteBuilder.ensureLength(3)
        val arr = byteBuilder.arr; val length = byteBuilder.length
        arr(length) = MPK.Int16.toByte
        writeUInt16(arr, length, i)
        byteBuilder.length = length + 3
      } else{
        byteBuilder.ensureLength(5)
        val arr = byteBuilder.arr; val length = byteBuilder.length
        arr(length) = MPK.Int32.toByte
        writeUInt32(arr, length, i)
        byteBuilder.length = length + 5
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
      val arr = byteBuilder.arr; val length = byteBuilder.length
      arr(length) = MPK.UInt32.toByte
      writeUInt32(arr, length, i.toInt)
      byteBuilder.length = length + 5
    }else{
      byteBuilder.ensureLength(9)
      val arr = byteBuilder.arr; val length = byteBuilder.length
      arr(length) = MPK.Int64.toByte
      writeUInt64(arr, length, i)
      byteBuilder.length = length + 9
    }
    flushElemBuilder()
    out
  }

  override def visitUInt64(i: Long, index: Int) = {
    if (i >= 0) visitInt64(i, index)
    else{
      byteBuilder.ensureLength(9)
      val arr = byteBuilder.arr; val length = byteBuilder.length
      arr(length) = MPK.UInt64.toByte
      writeUInt64(arr, length, i)
      byteBuilder.length = length + 9
    }
    flushElemBuilder()
    out
  }

  override def visitString(s: CharSequence, index: Int) = {
    val strBytes = s.toString.getBytes(java.nio.charset.StandardCharsets.UTF_8)
    val strLength = strBytes.length
    if (strLength <= 31){
      byteBuilder.ensureLength(1 + strLength)
      val arr = byteBuilder.arr; val length = byteBuilder.length
      arr(length) = (MPK.FixStrMask | strLength).toByte
      byteBuilder.length = length + 1
    } else if (strLength <= 255){
      byteBuilder.ensureLength(2 + strLength)
      val arr = byteBuilder.arr; val length = byteBuilder.length
      arr(length) = MPK.Str8.toByte
      writeUInt8(arr, length, strLength)
      byteBuilder.length = length + 2
    }else if (strLength <= 65535){
      byteBuilder.ensureLength(3 + strLength)
      val arr = byteBuilder.arr; val length = byteBuilder.length
      arr(length) = MPK.Str16.toByte
      writeUInt16(arr, length, strLength)
      byteBuilder.length = length + 3
    }else {
      byteBuilder.ensureLength(5 + strLength)
      val arr = byteBuilder.arr; val length = byteBuilder.length
      arr(length) = MPK.Str32.toByte
      writeUInt32(arr, length, strLength)
      byteBuilder.length = length + 5
    }

    byteBuilder.appendAll(strBytes, strLength)
    flushElemBuilder()
    out
  }
  override def visitBinary(bytes: Array[Byte], offset: Int, len: Int, index: Int) = {
    if (len <= 255) {
      byteBuilder.ensureLength(2)
      val arr = byteBuilder.arr; val length = byteBuilder.length
      arr(length) = MPK.Bin8.toByte
      writeUInt8(arr, length, len)
      byteBuilder.length = length + 2
    } else if (len <= 65535) {
      byteBuilder.ensureLength(3)
      val arr = byteBuilder.arr; val length = byteBuilder.length
      arr(length) = MPK.Bin16.toByte
      writeUInt16(arr, length, len)
      byteBuilder.length = length + 3
    } else {
      byteBuilder.ensureLength(5)
      val arr = byteBuilder.arr; val length = byteBuilder.length
      arr(length) = MPK.Bin32.toByte
      writeUInt32(arr, length, len)
      byteBuilder.length = length + 5
    }

    byteBuilder.appendAll(bytes, offset, len)
    flushElemBuilder()
    out
  }

  def writeUInt8(arr: Array[Byte], length: Int, i: Int): Unit = arr(length + 1) = i.toByte

  def writeUInt16(arr: Array[Byte], length: Int, i: Int): Unit = {
    arr(length + 1) = ((i >> 8) & 0xff).toByte
    arr(length + 2) = ((i >> 0) & 0xff).toByte
  }
  def writeUInt32(arr: Array[Byte], length: Int, i: Int): Unit = {
    arr(length + 1) = ((i >> 24) & 0xff).toByte
    arr(length + 2) = ((i >> 16) & 0xff).toByte
    arr(length + 3) = ((i >> 8) & 0xff).toByte
    arr(length + 4) = ((i >> 0) & 0xff).toByte
  }
  def writeUInt64(arr: Array[Byte], length: Int, i: Long): Unit = {
    arr(length + 1) = ((i >> 56) & 0xff).toByte
    arr(length + 2) = ((i >> 48) & 0xff).toByte
    arr(length + 3) = ((i >> 40) & 0xff).toByte
    arr(length + 4) = ((i >> 32) & 0xff).toByte
    arr(length + 5) = ((i >> 24) & 0xff).toByte
    arr(length + 6) = ((i >> 16) & 0xff).toByte
    arr(length + 7) = ((i >> 8) & 0xff).toByte
    arr(length + 8) = ((i >> 0) & 0xff).toByte
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
          val arr = byteBuilder.arr; val length = byteBuilder.length
          arr(length) = MPK.Ext8.toByte
          writeUInt8(arr, length, len)
          byteBuilder.length = length + 2
        }else if (len <= 65535){
          byteBuilder.ensureLength(3)
          val arr = byteBuilder.arr; val length = byteBuilder.length
          arr(length) = MPK.Ext16.toByte
          writeUInt16(arr, length, len)
          byteBuilder.length = length + 3
        }else{
          byteBuilder.ensureLength(5)
          val arr = byteBuilder.arr; val length = byteBuilder.length
          writeUInt32(arr, length, len)
          arr(length) = MPK.Ext32.toByte
          byteBuilder.length = length + 5
        }
    }
    byteBuilder.append(tag)
    byteBuilder.appendAll(bytes, offset, len)
    flushElemBuilder()
    out
  }

  def visitChar(s: Char, index: Int) = {
    byteBuilder.ensureLength(3)
    val arr = byteBuilder.arr; val length = byteBuilder.length
    arr(length) = MPK.UInt16.toByte
    writeUInt16(arr, length, s)
    byteBuilder.length = length + 3
    flushElemBuilder()
    out
  }


  @deprecated("Not used, kept for binary compatibility")
  def writeUInt8(i: Int): Unit = writeUInt8(byteBuilder.arr, byteBuilder.length, i)

  @deprecated("Not used, kept for binary compatibility")
  def writeUInt16(i: Int): Unit = writeUInt16(byteBuilder.arr, byteBuilder.length, i)

  @deprecated("Not used, kept for binary compatibility")
  def writeUInt32(i: Int): Unit = writeUInt32(byteBuilder.arr, byteBuilder.length, i)

  @deprecated("Not used, kept for binary compatibility")
  def writeUInt64(i: Long): Unit = writeUInt64(byteBuilder.arr, byteBuilder.length, i)
}
