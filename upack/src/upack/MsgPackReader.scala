package upack
import upickle.core.Visitor
import upack.{MsgPackKeys => MPK}
class MsgPackReader[T](var index: Int = 0, input: Array[Byte], visitor: Visitor[Any, T]) {
  def parse(): T = {
    input(index) match{
      // positive fixint
      case x if x <= MPK.PositiveFixInt => index += 1; visitor.visitInt32(x & 0x7f, -1)

      case x if x <= MPK.FixMap=>
        val n = x & 0x0f
        index += 1
        parseMap(n)

      case x if x <= MPK.FixArray =>
        val n = x & 0x0f
        index += 1
        parseArray(n)

      case x if x <= MPK.FixStr =>
        val n = x & 0x1f
        index += 1
        parseStr(n)

      case MPK.Nil => index += 1; visitor.visitNull(-1)
      case MPK.False => index += 1; visitor.visitFalse(-1)
      case MPK.True => index += 1; visitor.visitTrue(-1)
      case MPK.Bin8 => parseBin(parseUInt8(index + 1))
      case MPK.Bin16 => parseBin(parseUInt16(index + 1))
      case MPK.Bin32 => parseBin(parseUInt32(index + 1))

//      case 0xc7 => visitor.visitExt8()
//      case 0xc8 => visitor.visitExt16()
//      case 0xc9 => visitor.visitExt32()
      case MPK.Float32 => visitor.visitNumRaw(java.lang.Float.intBitsToFloat(parseUInt32(index + 1)), -1)
      case MPK.Float64 => visitor.visitNumRaw(java.lang.Double.longBitsToDouble(parseUInt64(index + 1)), -1)
      case MPK.UInt8 => visitor.visitInt32(parseUInt8(index + 1), -1)
      case MPK.UInt16 => visitor.visitInt32(parseUInt16(index + 1), -1)
      case MPK.UInt32 => visitor.visitInt32(parseUInt32(index + 1), -1)
      case MPK.UInt64 => visitor.visitInt64(parseUInt64(index + 1), -1)
      case MPK.Int8 => visitor.visitInt32(parseUInt8(index + 1), -1)
      case MPK.Int16 => visitor.visitInt32(parseUInt16(index + 1), -1)
      case MPK.Int32 => visitor.visitInt32(parseUInt32(index + 1), -1)
      case MPK.Int64 => visitor.visitInt64(parseUInt64(index + 1), -1)

//      case 0xd4 => visitor.visitFixExt1()
//      case 0xd5 => visitor.visitFixExt2()
//      case 0xd6 => visitor.visitFixExt4()
//      case 0xd7 => visitor.visitFixExt8()
//      case 0xd8 => visitor.visitFixExt16()
      case MPK.Str8 => parseStr(parseUInt8(index + 1))
      case MPK.Str16 => parseStr(parseUInt16(index + 1))
      case MPK.Str32=> parseStr(parseUInt32(index + 1))

      case MPK.Array16 => parseArray(parseUInt16(index + 1))
      case MPK.Array32 => parseArray(parseUInt32(index + 1))

      case MPK.Map16 => parseMap(parseUInt16(index + 1))
      case MPK.Map32 => parseMap(parseUInt32(index + 1))
      // negative fixint
      case x if x >= 0xe0 => index += 1; visitor.visitInt32(-(x & 0x1f), -1)
    }
  }
  def parseStr(n: Int) = {
    val res = visitor.visitString(new String(input, index , n), -1)
    index += n
    res
  }
  def parseBin(n: Int) = {
    val res = visitor.visitBin(input, index, n, -1)
    index += n
    res
  }
  def parseMap(n: Int) = {
    val obj = visitor.visitObject(n, -1)
    var i = 0
    while(i < n){
      obj.visitKey(parse().toString, -1)
      obj.visitValue(parse(), -1)
      i += 1
    }
    obj.visitEnd(-1)
  }
  def parseArray(n: Int) = {
    val arr = visitor.visitArray(n, -1)
    var i = 0
    while(i < n){
      arr.visitValue(parse(), -1)
      i += 1
    }
    arr.visitEnd(-1)
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
    (input(i) & 0xff) << 56 | (input(i + 1) & 0xff) << 48 | (input(i + 2) & 0xff) << 40 | (input(i + 3) & 0xff) << 32 |
    (input(i + 4) & 0xff) << 24 | (input(i + 5) & 0xff) << 16 | (input(i + 6) & 0xff) << 8 | (input(i + 7) & 0xff) << 0
  }
}
