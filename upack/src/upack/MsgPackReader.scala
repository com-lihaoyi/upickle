package upack
import upickle.core.{Visitor, UberBuffer}
import upack.{MsgPackKeys => MPK}

import scala.annotation.switch

class MsgPackReader(startIndex: Int = 0, input0: Array[Byte]) extends BaseMsgPackReader {
  private[this] var index0 = startIndex
  def incrementIndex(i: Int): Unit = index0 += i
  def setIndex(i: Int): Unit = index0 = i
  def sliceString(i: Int, n: Int): String = new String(input0, i, n)
  def sliceBytes(i: Int, n: Int): (Array[Byte], Int, Int) = (input0, i, n)
  def index = index0
  def input(i: Int): Byte = input0(i)
  def dropBufferUntil(i: Int): Unit = ()//donothing
}
class InputStreamMsgPackReader(input0: java.io.InputStream, bufferSize: Int) extends BaseMsgPackReader {
  private[this] val buffer: UberBuffer = new UberBuffer(16)
  private[this] val streamBuffer = new Array[Byte](bufferSize)
  private[this] var firstIdx: Int = 0 // index in the data corresponding to the 0th element in the buffer
  private[this] var index0 = 0

  def incrementIndex(i: Int): Unit = {
    index0 += i
  }
  def setIndex(i: Int): Unit = {
    index0 = i
  }
  def sliceString(i: Int, n: Int): String = {
    requestUntil(i + n)
    new String(buffer.slice(i - firstIdx, i + n - firstIdx))
  }
  def sliceBytes(i: Int, n: Int): (Array[Byte], Int, Int) = {
    requestUntil(i + n)
    val arr = buffer.slice(i - firstIdx, i + n - firstIdx)
    (arr, 0, arr.length)
  }
  def index = index0
  def input(i: Int): Byte = {
    requestUntil(i)
    buffer.apply(i - firstIdx)
  }

  def length: Int = firstIdx + buffer.length
  def requestUntil(until: Int): Unit = {
    var done = false
    while (length <= until && !done) {
      input0.read(streamBuffer, 0, bufferSize) match{
        case -1 => done = true
        case n => buffer.write(streamBuffer, n)
      }
    }
  }
  def dropBufferUntil(i: Int): Unit = {
    if (i > firstIdx){
      buffer.drop(i  - firstIdx)
      firstIdx = i
    }
  }
}

abstract class BaseMsgPackReader{

  def index: Int
  def input(i: Int): Byte
  def incrementIndex(i: Int): Unit
  def setIndex(i: Int): Unit
  def sliceString(i: Int, j: Int): String
  def sliceBytes(i: Int, j: Int): (Array[Byte], Int, Int)
  def dropBufferUntil(i: Int): Unit
  def parse[T](visitor: Visitor[_, T]): T = {
    dropBufferUntil(index)
    val n = input(index)
    (n & 0xFF: @switch) match{
      case MPK.Nil => incrementIndex(1); visitor.visitNull(index)
      case MPK.False => incrementIndex(1); visitor.visitFalse(index)
      case MPK.True => incrementIndex(1); visitor.visitTrue(index)

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

      case MPK.FixExt1 => incrementIndex(1); parseExt(1, visitor)
      case MPK.FixExt2 => incrementIndex(1); parseExt(2, visitor)
      case MPK.FixExt4 => incrementIndex(1); parseExt(4, visitor)
      case MPK.FixExt8 => incrementIndex(1); parseExt(8, visitor)
      case MPK.FixExt16 => incrementIndex(1); parseExt(16, visitor)

      case MPK.Str8 => parseStr(parseUInt8(index + 1), visitor)
      case MPK.Str16 => parseStr(parseUInt16(index + 1), visitor)
      case MPK.Str32=> parseStr(parseUInt32(index + 1), visitor)

      case MPK.Array16 => parseArray(parseUInt16(index + 1), visitor)
      case MPK.Array32 => parseArray(parseUInt32(index + 1), visitor)

      case MPK.Map16 => parseMap(parseUInt16(index + 1), visitor)
      case MPK.Map32 => parseMap(parseUInt32(index + 1), visitor)
      case x =>
        if (x <= MPK.PositiveFixInt) {
          // positive fixint
          incrementIndex(1)
          visitor.visitInt32(x & 0x7f, index)
        } else if (x <= MPK.FixMap) {
          val n = x & 0x0f
          incrementIndex(1)
          parseMap (n, visitor)
        } else if (x <= MPK.FixArray) {
          val n = x & 0x0f
          incrementIndex(1)
          parseArray (n, visitor)
        }
          else if (x <= MPK.FixStr ) {
          val n = x & 0x1f
          incrementIndex(1)
          parseStr (n, visitor)
        } else if (x >= 0xe0) { // negative fixint
          incrementIndex(1)
          visitor.visitInt32 (x | 0xffffffe0, index)
        } else ???
    }
  }
  def parseExt[T](n: Int, visitor: Visitor[_, T]) = {
    val (arr, i, j) = sliceBytes(index + 1, n)
    visitor.visitExt(input(index), arr, i, j, index)
  }

  def parseStr[T](n: Int, visitor: Visitor[_, T]) = {
    val res = visitor.visitString(sliceString(index, n), index)
    incrementIndex(n)
    res
  }
  def parseBin[T](n: Int, visitor: Visitor[_, T]) = {
    val (arr, i, j) = sliceBytes(index, n)
    val res = visitor.visitBinary(arr, i, j, index)
    incrementIndex(n)
    res
  }
  def parseMap[T](n: Int, visitor: Visitor[_, T]) = {
    val obj = visitor.visitObject(n, index)

    var i = 0
    while(i < n){
      val keyVisitor = obj.visitKey(index)
      obj.visitKeyValue(parse(keyVisitor.asInstanceOf[Visitor[_, T]]))
      obj.narrow.visitValue(parse(obj.subVisitor.asInstanceOf[Visitor[_, T]]), index)
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
      i += 1
    }
    arr.visitEnd(index)
  }
  def parseUInt8(i: Int) = {
    setIndex(i + 1)
    input(i) & 0xff
  }
  def parseUInt16(i: Int) = {
    setIndex(i + 2)
    (input(i) & 0xff) << 8 | input(i + 1) & 0xff
  }
  def parseUInt32(i: Int) = {
    setIndex(i + 4)
    (input(i) & 0xff) << 24 | (input(i + 1) & 0xff) << 16 | (input(i + 2) & 0xff) << 8 | input(i + 3) & 0xff
  }
  def parseUInt64(i: Int) = {
    setIndex(i + 8)
    (input(i + 0).toLong & 0xff) << 56 | (input(i + 1).toLong & 0xff) << 48 |
    (input(i + 2).toLong & 0xff) << 40 | (input(i + 3).toLong & 0xff) << 32 |
    (input(i + 4).toLong & 0xff) << 24 | (input(i + 5).toLong & 0xff) << 16 |
    (input(i + 6).toLong & 0xff) << 8 | (input(i + 7).toLong & 0xff) << 0
  }
}
