package ujson.borer.internal


import ujson.borer._

/**
  * A [[Receiver]] which simply buffers all incoming data in fields of the appropriate type,
  * for easy querying from the outside.
  */
final private[borer] class Receptacle extends Receiver with java.lang.Cloneable {

  private[this] var _bool: Boolean  = _
  private[this] var _int: Int       = _
  private[this] var _long: Long     = _
  private[this] var _float: Float   = _
  private[this] var _double: Double = _
  private[this] var _obj: Any       = _

  private[this] var _byteAccess: ByteAccess[Any] = _

  @inline def boolValue: Boolean        = _bool
  @inline def intValue: Int             = _int
  @inline def longValue: Long           = _long
  @inline def floatValue: Float         = _float
  @inline def doubleValue: Double       = _double
  @inline def stringValue: String       = _obj.asInstanceOf[String]
  @inline def charBufValue: Array[Char] = _obj.asInstanceOf[Array[Char]]
  @inline def tagValue: Tag             = _obj.asInstanceOf[Tag]

  @inline def getBytes[Bytes](implicit byteAccess: ByteAccess[Bytes]): Bytes =
    byteAccess.convert(_obj)(_byteAccess)

  def onNull(): Unit = ()

  def onUndefined(): Unit = ()

  def onBoolean(value: Boolean): Unit = _bool = value

  def onInt(value: Int): Unit = _int = value

  def onLong(value: Long): Unit = _long = value

  def onOverLong(negative: Boolean, value: Long): Unit = {
    _bool = negative
    _long = value
  }

  def onFloat16(value: Float): Unit = _float = value

  def onFloat(value: Float): Unit = _float = value

  def onDouble(value: Double): Unit = _double = value

  def onNumberString(value: String): Unit = _obj = value

  def onBytes[Bytes](value: Bytes)(implicit byteAccess: ByteAccess[Bytes]): Unit = {
    _obj = value
    _byteAccess = byteAccess.asInstanceOf[ByteAccess[Any]]
  }

  def onBytesStart(): Unit = ()

  def onString(value: String): Unit = _obj = value

  def onChars(buffer: Array[Char]): Unit = onChars(buffer, buffer.length)

  def onChars(buffer: Array[Char], length: Int): Unit = {
    _obj = buffer
    _int = length
  }

  def onText[Bytes](value: Bytes)(implicit byteAccess: ByteAccess[Bytes]): Unit = {
    _obj = value
    _byteAccess = byteAccess.asInstanceOf[ByteAccess[Any]]
  }

  def onTextStart(): Unit = ()

  def onArrayHeader(length: Long): Unit = _long = length

  def onArrayStart(): Unit = ()

  def onMapHeader(length: Long): Unit = _long = length

  def onMapStart(): Unit = ()

  def onBreak(): Unit = ()

  def onTag(value: Tag): Unit = _obj = value

  def onSimpleValue(value: Int): Unit = _int = value

  def onEndOfInput(): Unit = ()

  def pushInto(receiver: Receiver, dataItem: Int): Unit =
    Integer.numberOfTrailingZeros(dataItem) match {
      case DataItem.Shifts.Null         => receiver.onNull()
      case DataItem.Shifts.Undefined    => receiver.onUndefined()
      case DataItem.Shifts.Boolean      => receiver.onBoolean(_bool)
      case DataItem.Shifts.Int          => receiver.onInt(_int)
      case DataItem.Shifts.Long         => receiver.onLong(_long)
      case DataItem.Shifts.OverLong     => receiver.onOverLong(_bool, _long)
      case DataItem.Shifts.Float16      => receiver.onFloat16(_float)
      case DataItem.Shifts.Float        => receiver.onFloat(_float)
      case DataItem.Shifts.Double       => receiver.onDouble(_double)
      case DataItem.Shifts.NumberString => receiver.onNumberString(_obj.asInstanceOf[String])
      case DataItem.Shifts.String       => receiver.onString(_obj.asInstanceOf[String])
      case DataItem.Shifts.Chars        => receiver.onChars(_obj.asInstanceOf[Array[Char]], _int)
      case DataItem.Shifts.Text         => receiver.onText[Any](_obj)(_byteAccess)
      case DataItem.Shifts.TextStart    => receiver.onTextStart()
      case DataItem.Shifts.Bytes        => receiver.onBytes[Any](_obj)(_byteAccess)
      case DataItem.Shifts.BytesStart   => receiver.onBytesStart()
      case DataItem.Shifts.ArrayHeader  => receiver.onArrayHeader(_long)
      case DataItem.Shifts.ArrayStart   => receiver.onArrayStart()
      case DataItem.Shifts.MapHeader    => receiver.onMapHeader(_long)
      case DataItem.Shifts.MapStart     => receiver.onMapStart()
      case DataItem.Shifts.Break        => receiver.onBreak()
      case DataItem.Shifts.Tag          => receiver.onTag(_obj.asInstanceOf[Tag])
      case DataItem.Shifts.SimpleValue  => receiver.onSimpleValue(_int)
      case DataItem.Shifts.EndOfInput   => receiver.onEndOfInput()
    }
}