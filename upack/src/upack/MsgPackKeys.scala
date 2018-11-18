package upack

object MsgPackKeys {
  final val PositiveFixInt = 0x7f
  final val FixMapMask= 0x80
  final val FixMap = 0x8f
  final val FixArrMask = 0x90
  final val FixArray = 0x9f
  final val FixStrMask = 0xa0
  final val FixStr = 0xbf

  final val Nil = 0xc0
  final val False = 0xc2
  final val True = 0xc3

  final val Bin8 = 0xc4
  final val Bin16 = 0xc5
  final val Bin32 = 0xc6

  final val Ext8 = 0xc7
  final val Ext16 = 0xc8
  final val Ext32 = 0xc9

  final val Float32 = 0xca
  final val Float64 = 0xcb

  final val UInt8 = 0xcc
  final val UInt16 = 0xcd
  final val UInt32 = 0xce
  final val UInt64 = 0xcf

  final val Int8 = 0xd0
  final val Int16 = 0xd1
  final val Int32 = 0xd2
  final val Int64 = 0xd3

  final val FixExt1 = 0xd4
  final val FixExt2 = 0xd5
  final val FixExt4 = 0xd6
  final val FixExt8 = 0xd7
  final val FixExt16 = 0xd8

  final val Str8 = 0xd9
  final val Str16 = 0xda
  final val Str32 = 0xdb

  final val Array16 = 0xdc
  final val Array32 = 0xdd

  final val Map16 = 0xde
  final val Map32 = 0xdf
}
