package upack
import upickle.core.{BufferingInputStreamParser, Visitor}
import upack.{MsgPackKeys => MPK}

import scala.annotation.switch

class MsgPackReader(input0: Array[Byte]) extends BaseMsgPackReader {
  val srcLength = input0.length
  protected[this] final def close() = {}

  // Make sure we never call this method, since it will mutate the original array,
  // and it should not be necessary to call it if our implementation is correct.
  override def growBuffer(until: Int): Unit = ???

  def readDataIntoBuffer(buffer: Array[Byte], bufferOffset: Int) = {
    if(buffer == null) (input0, false, srcLength)
    else (input0, true, -1)
  }
}

class InputStreamMsgPackReader(val inputStream: java.io.InputStream,
                               val minBufferStartSize: Int = BufferingInputStreamParser.defaultMinBufferStartSize,
                               val maxBufferStartSize: Int = BufferingInputStreamParser.defaultMaxBufferStartSize)
extends BaseMsgPackReader with upickle.core.BufferingInputStreamParser{
}

abstract class BaseMsgPackReader extends upickle.core.BufferingByteParser{

  private[this] var index = 0
  def getIndex = index
  def parse[T](visitor: Visitor[_, T]): T = {

    val n = getByteSafe(index)
    (n & 0xFF: @switch) match{
      case MPK.Nil => index += 1; visitor.visitNull(index)
      case MPK.False => index += 1; visitor.visitFalse(index)
      case MPK.True => index += 1; visitor.visitTrue(index)

      case MPK.Bin8 => parseBin(parseUInt8(index + 1), visitor)
      case MPK.Bin16 => parseBin(parseUInt16(index + 1), visitor)
      case MPK.Bin32 => parseBin(parseUInt32(index + 1), visitor)

      case MPK.Ext8 => parseExt(parseUInt8(index + 1), visitor)
      case MPK.Ext16 => parseExt(parseUInt16(index + 1), visitor)
      case MPK.Ext32 => parseExt(parseUInt32(index + 1), visitor)

      case MPK.Float32 => visitor.visitFloat64(java.lang.Float.intBitsToFloat(parseUInt32(index + 1)), index)
      case MPK.Float64 => visitor.visitFloat64(java.lang.Double.longBitsToDouble(parseUInt64(index + 1)), index)

      case MPK.UInt8 => visitor.visitInt32(parseUInt8(index + 1), index)
      case MPK.UInt16 => visitor.visitInt32(parseUInt16(index + 1), index)
      case MPK.UInt32 => visitor.visitInt64(parseUInt32(index + 1) & 0xffffffffL, index)
      case MPK.UInt64 => visitor.visitUInt64(parseUInt64(index + 1), index)

      case MPK.Int8 => visitor.visitInt32(parseUInt8(index + 1).toByte, index)
      case MPK.Int16 => visitor.visitInt32(parseUInt16(index + 1).toShort, index)
      case MPK.Int32 => visitor.visitInt32(parseUInt32(index + 1), index)
      case MPK.Int64 => visitor.visitInt64(parseUInt64(index + 1), index)

      case MPK.FixExt1 => index += 1; parseExt(1, visitor)
      case MPK.FixExt2 => index += 1; parseExt(2, visitor)
      case MPK.FixExt4 => index += 1; parseExt(4, visitor)
      case MPK.FixExt8 => index += 1; parseExt(8, visitor)
      case MPK.FixExt16 => index += 1; parseExt(16, visitor)

      case MPK.Str8 => parseStr(parseUInt8(index + 1), visitor)
      case MPK.Str16 => parseStr(parseUInt16(index + 1), visitor)
      case MPK.Str32 => parseStr(parseUInt32(index + 1), visitor)

      case MPK.Array16 => parseArray(parseUInt16(index + 1), visitor)
      case MPK.Array32 => parseArray(parseUInt32(index + 1), visitor)

      case MPK.Map16 => parseMap(parseUInt16(index + 1), visitor)
      case MPK.Map32 => parseMap(parseUInt32(index + 1), visitor)
      case x =>
        if (x <= MPK.PositiveFixInt) {
          // positive fixint
          index += 1
          visitor.visitInt32(x & 0x7f, index)
        } else if (x <= MPK.FixMap) {
          val n = x & 0x0f
          index += 1
          parseMap (n, visitor)
        } else if (x <= MPK.FixArray) {
          val n = x & 0x0f
          index += 1
          parseArray (n, visitor)
        }
          else if (x <= MPK.FixStr ) {
          val n = x & 0x1f
          index += 1
          parseStr(n, visitor)
        } else if (x >= 0xe0) { // negative fixint
          index += 1
          visitor.visitInt32(x | 0xffffffe0, index)
        } else ???
    }
  }
  def parseExt[T](n: Int, visitor: Visitor[_, T]) = {
    val (arr, i, j) = sliceArr(index + 1, n)
    visitor.visitExt(getByteSafe(index), arr, i, j, index)
  }

  def parseStr[T](n: Int, visitor: Visitor[_, T]) = {
    val res = visitor.visitString(sliceString(index, index + n), index)
    index += n
    res
  }
  def parseBin[T](n: Int, visitor: Visitor[_, T]) = {
    val (arr, i, j) = sliceArr(index, n)
    val res = visitor.visitBinary(arr, i, j, index)
    index += n
    res
  }
  def parseMap[T](n: Int, visitor: Visitor[_, T]) = {
    val obj = visitor.visitObject(n, index)

    var i = 0
    while(i < n){
      val keyVisitor = obj.visitKey(index)
      obj.visitKeyValue(parse(keyVisitor.asInstanceOf[Visitor[_, T]]))
      obj.narrow.visitValue(parse(obj.subVisitor.asInstanceOf[Visitor[_, T]]), index)
      dropBufferUntil(index)
      i += 1
    }
    obj.visitEnd(index)
  }
  def parseArray[T](n: Int, visitor: Visitor[_, T]) = {
    val arr = visitor.visitArray(n, index)

    var i = 0

    while(i < n){
      val v = parse(arr.subVisitor.asInstanceOf[Visitor[_, T]])
      arr.narrow.visitValue(v, index)
      dropBufferUntil(index)
      i += 1
    }
    arr.visitEnd(index)
  }
  def parseUInt8(i: Int) = {
    index = i + 1
    getByteSafe(i) & 0xff
  }
  def parseUInt16(i: Int) = {
    index = i + 2
    requestUntil(i + 1)
    (getByteUnsafe(i) & 0xff) << 8 | getByteUnsafe(i + 1) & 0xff
  }
  def parseUInt32(i: Int) = {
    index = i + 4
    requestUntil(i + 3)
    (getByteUnsafe(i) & 0xff) << 24 | (getByteUnsafe(i + 1) & 0xff) << 16 |
    (getByteUnsafe(i + 2) & 0xff) << 8 | getByteUnsafe(i + 3) & 0xff
  }
  def parseUInt64(i: Int) = {
    index = i + 8
    requestUntil(i + 7)
    (getByteUnsafe(i + 0).toLong & 0xff) << 56 | (getByteUnsafe(i + 1).toLong & 0xff) << 48 |
    (getByteUnsafe(i + 2).toLong & 0xff) << 40 | (getByteUnsafe(i + 3).toLong & 0xff) << 32 |
    (getByteUnsafe(i + 4).toLong & 0xff) << 24 | (getByteUnsafe(i + 5).toLong & 0xff) << 16 |
    (getByteUnsafe(i + 6).toLong & 0xff) << 8 | (getByteUnsafe(i + 7).toLong & 0xff) << 0
  }
}
