package ujson.borer


/**
  * A simple mapping of CBOR data item types to bits for efficient masking.
  */
object DataItem {

  object Shifts {
    final val Null         = 0
    final val Undefined    = 1
    final val Boolean      = 2
    final val Int          = 3
    final val Long         = 4
    final val OverLong     = 5
    final val Float16      = 6
    final val Float        = 7
    final val Double       = 8
    final val NumberString = 9
    final val String       = 10
    final val Chars        = 11
    final val Text         = 12
    final val TextStart    = 13
    final val Bytes        = 14
    final val BytesStart   = 15
    final val ArrayHeader  = 16
    final val ArrayStart   = 17
    final val MapHeader    = 18
    final val MapStart     = 19
    final val Break        = 20
    final val Tag          = 21
    final val SimpleValue  = 22
    final val EndOfInput   = 23
  }

  //////////////////////////////// BASIC ////////////////////////////////

  final val Null         = 1 << Shifts.Null
  final val Undefined    = 1 << Shifts.Undefined
  final val Boolean      = 1 << Shifts.Boolean
  final val Int          = 1 << Shifts.Int
  final val Long         = 1 << Shifts.Long
  final val OverLong     = 1 << Shifts.OverLong
  final val Float16      = 1 << Shifts.Float16
  final val Float        = 1 << Shifts.Float
  final val Double       = 1 << Shifts.Double
  final val NumberString = 1 << Shifts.NumberString
  final val String       = 1 << Shifts.String
  final val Chars        = 1 << Shifts.Chars
  final val Text         = 1 << Shifts.Text
  final val TextStart    = 1 << Shifts.TextStart
  final val Bytes        = 1 << Shifts.Bytes
  final val BytesStart   = 1 << Shifts.BytesStart
  final val ArrayHeader  = 1 << Shifts.ArrayHeader
  final val ArrayStart   = 1 << Shifts.ArrayStart
  final val MapHeader    = 1 << Shifts.MapHeader
  final val MapStart     = 1 << Shifts.MapStart
  final val Break        = 1 << Shifts.Break
  final val Tag          = 1 << Shifts.Tag
  final val SimpleValue  = 1 << Shifts.SimpleValue
  final val EndOfInput   = 1 << Shifts.EndOfInput

  //////////////////////////////// COMPOUND ////////////////////////////////

  final val None        = 0
  final val Complex     = ArrayHeader | ArrayStart | MapHeader | MapStart | BytesStart | TextStart
  final val AllButBreak = ((EndOfInput << 1) - 1) & ~Break

  //////////////////////////////////////////////////////////////////////////

  def stringify(mask: Int): String =
    if (mask != AllButBreak) {
      Iterator
        .range(0, 25)
        .map { i =>
          mask & (1 << i) match {
            case None => ""

            case Null      => "Null"
            case Undefined => "Undefined"
            case Boolean   => "Bool"

            case Int          => "Int"
            case Long         => "Long"
            case OverLong     => "OverLong"
            case Float16      => "Float16"
            case Float        => "Float"
            case Double       => "Double"
            case NumberString => "NumberString"

            case String    => "String"
            case Chars     => "Chars"
            case Text      => "Text"
            case TextStart => "Start of unbounded Text"

            case Bytes      => "Bytes"
            case BytesStart => "Start of unbounded Bytes"

            case ArrayHeader => "Array"
            case ArrayStart  => "Start of unbounded Array"

            case MapHeader => "Map"
            case MapStart  => "Start of unbounded Map"

            case Break => "BREAK"
            case Tag   => "Tag"

            case SimpleValue => "Simple Value"

            case EndOfInput => "End of Input"
          }
        }
        .filter(_.nonEmpty)
        .take(java.lang.Integer.bitCount(mask))
        .toList match {
        case Nil      => "none"
        case x :: Nil => x
        case x        => x.init.mkString("", ", ", " or " + x.last)
      }
    } else "Any DataItem except BREAK"
}
