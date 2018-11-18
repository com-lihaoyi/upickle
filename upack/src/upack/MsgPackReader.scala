package upack
import upickle.core.Visitor
import upack.{MsgPackKeys => MPK}

import scala.annotation.switch
class MsgPackReader(index0: Int = 0, input: Array[Byte]) {
  private[this] var index = index0
  def parse[T](visitor: Visitor[_, T]): T = {
    (input(index) & 0xFF: @switch) match{
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
      case MPK.Str32=> parseStr(parseUInt32(index + 1), visitor)

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
          parseStr (n, visitor)
        } else if (x >= 0xe0) { // negative fixint
          index += 1
          visitor.visitInt32 (x | 0xffffffe0, index)
        } else ???
    }
  }
  def parseExt[T](n: Int, visitor: Visitor[_, T]) = {
    visitor.visitExt(input(index), input, index + 1, n, index)
  }
  def parseKey(): String = {
    val x = input(index) & 0xFF
    if (x <= MPK.FixStr) {
      val n = x & 0x1f
      index += 1
      val res = new String(input, index, n)
      index += n
      res
    }else (x: @switch) match {
      case MPK.Str8 =>
        val n = parseUInt8(index + 1)
        val res = new String(input, index, n)
        index += n
        res
      case MPK.Str16 =>
        val n = parseUInt16(index + 1)
        val res = new String(input, index, n)
        index += n
        res
      case MPK.Str32=>
        val n = parseUInt32(index + 1)
        val res = new String(input, index, n)
        index += n
        res
    }
  }

  def parseStr[T](n: Int, visitor: Visitor[_, T]) = {
    val res = visitor.visitString(new String(input, index , n), index)
    index += n
    res
  }
  def parseBin[T](n: Int, visitor: Visitor[_, T]) = {
    val res = visitor.visitBin(input, index, n, index)
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
    index = i + 1
    input(i) & 0xff
  }
  def parseUInt16(i: Int) = {
    index = i + 2
    (input(i) & 0xff) << 8 | input(i + 1) & 0xff
  }
  def parseUInt32(i: Int) = {
    index = i + 4
    (input(i) & 0xff) << 24 | (input(i + 1) & 0xff) << 16 | (input(i + 2) & 0xff) << 8 | input(i + 3) & 0xff
  }
  def parseUInt64(i: Int) = {
    index = i + 8
    (input(i + 0).toLong & 0xff) << 56 | (input(i + 1).toLong & 0xff) << 48 |
    (input(i + 2).toLong & 0xff) << 40 | (input(i + 3).toLong & 0xff) << 32 |
    (input(i + 4).toLong & 0xff) << 24 | (input(i + 5).toLong & 0xff) << 16 |
    (input(i + 6).toLong & 0xff) << 8 | (input(i + 7).toLong & 0xff) << 0
  }
}
