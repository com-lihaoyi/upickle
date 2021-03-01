package ujson.borer.internal


import ujson.borer._

import scala.annotation.tailrec

final private[borer] class ElementDeque(maxBufferSize: Int, val next: ElementDeque) {
  import ujson.borer.DataItem.{Shifts => DIS}

  private[this] val byteBuffer = new ResizableByteRingBuffer(16, math.max(16, maxBufferSize))
  private[this] val objBuffer  = new ResizableRingBuffer[AnyRef](16, math.max(16, maxBufferSize))

  val prependReceiver: Receiver =
    new Receiver {
      def onNull(): Unit                  = ret(byteBuffer.prepend1(DIS.Null))
      def onUndefined(): Unit             = ret(byteBuffer.prepend1(DIS.Undefined))
      def onBoolean(value: Boolean): Unit = ret(byteBuffer.prepend2(DIS.Boolean, if (value) 1 else 0))
      def onInt(value: Int): Unit         = ret(byteBuffer.prepend5(DIS.Int, value))
      def onLong(value: Long): Unit       = ret(byteBuffer.prepend9(DIS.Long, value))

      def onOverLong(negative: Boolean, value: Long): Unit =
        ret(byteBuffer.prepend1(DIS.OverLong) && byteBuffer.prepend9(if (negative) 1 else 0, value))

      def onFloat16(value: Float): Unit       = ret(byteBuffer.prepend5(DIS.Float16, java.lang.Float.floatToIntBits(value)))
      def onFloat(value: Float): Unit         = ret(byteBuffer.prepend5(DIS.Float, java.lang.Float.floatToIntBits(value)))
      def onDouble(value: Double): Unit       = ret(byteBuffer.prepend9(DIS.Double, java.lang.Double.doubleToLongBits(value)))
      def onNumberString(value: String): Unit = ret(byteBuffer.prepend1(DIS.NumberString) && objBuffer.prepend(value))

      def onBytes[Bytes](value: Bytes)(implicit ba: ByteAccess[Bytes]): Unit =
        ret(byteBuffer.prepend1(DIS.Bytes) && objBuffer.prepend(ba) && objBuffer.prepend(value.asInstanceOf[AnyRef]))

      def onBytesStart(): Unit          = ret(byteBuffer.prepend1(DIS.BytesStart))
      def onString(value: String): Unit = ret(byteBuffer.prepend1(DIS.String) && objBuffer.prepend(value))

      def onChars(buffer: Array[Char], length: Int): Unit =
        ret(byteBuffer.prepend1(DIS.String) && objBuffer.prepend(new String(buffer, 0, length)))

      def onText[Bytes](value: Bytes)(implicit ba: ByteAccess[Bytes]): Unit =
        ret(byteBuffer.prepend1(DIS.Text) && objBuffer.prepend(ba) && objBuffer.prepend(value.asInstanceOf[AnyRef]))

      def onTextStart(): Unit               = ret(byteBuffer.prepend1(DIS.TextStart))
      def onArrayHeader(length: Long): Unit = ret(byteBuffer.prepend9(DIS.ArrayHeader, length))
      def onArrayStart(): Unit              = ret(byteBuffer.prepend1(DIS.ArrayStart))
      def onMapHeader(length: Long): Unit   = ret(byteBuffer.prepend9(DIS.MapHeader, length))
      def onMapStart(): Unit                = ret(byteBuffer.prepend1(DIS.MapStart))
      def onBreak(): Unit                   = ret(byteBuffer.prepend1(DIS.Break))
      def onTag(value: Tag): Unit           = ret(byteBuffer.prepend1(DIS.Tag) && objBuffer.prepend(value))
      def onSimpleValue(value: Int): Unit   = ret(byteBuffer.prepend5(DIS.SimpleValue, value))
      def onEndOfInput(): Unit              = ret(byteBuffer.prepend1(DIS.EndOfInput))
    }

  val appendReceiver: Receiver =
    new Receiver {
      def onNull(): Unit                  = ret(byteBuffer.append1(DIS.Null))
      def onUndefined(): Unit             = ret(byteBuffer.append1(DIS.Undefined))
      def onBoolean(value: Boolean): Unit = ret(byteBuffer.append2(DIS.Boolean, if (value) 1 else 0))
      def onInt(value: Int): Unit         = ret(byteBuffer.append5(DIS.Int, value))
      def onLong(value: Long): Unit       = ret(byteBuffer.append9(DIS.Long, value))

      def onOverLong(negative: Boolean, value: Long): Unit =
        ret(byteBuffer.append1(DIS.OverLong) && byteBuffer.append9(if (negative) 1 else 0, value))

      def onFloat16(value: Float): Unit       = ret(byteBuffer.append5(DIS.Float16, java.lang.Float.floatToIntBits(value)))
      def onFloat(value: Float): Unit         = ret(byteBuffer.append5(DIS.Float, java.lang.Float.floatToIntBits(value)))
      def onDouble(value: Double): Unit       = ret(byteBuffer.append9(DIS.Double, java.lang.Double.doubleToLongBits(value)))
      def onNumberString(value: String): Unit = ret(byteBuffer.append1(DIS.NumberString) && objBuffer.append(value))

      def onBytes[Bytes](value: Bytes)(implicit ba: ByteAccess[Bytes]): Unit =
        ret(byteBuffer.append1(DIS.Bytes) && objBuffer.append(value.asInstanceOf[AnyRef]) && objBuffer.append(ba))

      def onBytesStart(): Unit          = ret(byteBuffer.append1(DIS.BytesStart))
      def onString(value: String): Unit = ret(byteBuffer.append1(DIS.String) && objBuffer.append(value))

      def onChars(buffer: Array[Char], length: Int): Unit =
        ret(byteBuffer.append1(DIS.String) && objBuffer.append(new String(buffer, 0, length)))

      def onText[Bytes](value: Bytes)(implicit ba: ByteAccess[Bytes]): Unit =
        ret(byteBuffer.append1(DIS.Text) && objBuffer.append(value.asInstanceOf[AnyRef]) && objBuffer.append(ba))

      def onTextStart(): Unit               = ret(byteBuffer.append1(DIS.TextStart))
      def onArrayHeader(length: Long): Unit = ret(byteBuffer.append9(DIS.ArrayHeader, length))
      def onArrayStart(): Unit              = ret(byteBuffer.append1(DIS.ArrayStart))
      def onMapHeader(length: Long): Unit   = ret(byteBuffer.append9(DIS.MapHeader, length))
      def onMapStart(): Unit                = ret(byteBuffer.append1(DIS.MapStart))
      def onBreak(): Unit                   = ret(byteBuffer.append1(DIS.Break))
      def onTag(value: Tag): Unit           = ret(byteBuffer.append1(DIS.Tag) && objBuffer.append(value))
      def onSimpleValue(value: Int): Unit   = ret(byteBuffer.append5(DIS.SimpleValue, value))
      def onEndOfInput(): Unit              = ret(byteBuffer.append1(DIS.EndOfInput))
    }

  @inline def isEmpty: Boolean  = byteBuffer.isEmpty
  @inline def nonEmpty: Boolean = byteBuffer.nonEmpty

  @inline def clear(): Unit = {
    byteBuffer.clear()
    objBuffer.clear()
  }

  def pull(receiver: Receiver): Int = {
    val shift = byteBuffer.unsafeReadByte()
    shift match {
      case DIS.Null      => receiver.onNull()
      case DIS.Undefined => receiver.onUndefined()
      case DIS.Boolean   => receiver.onBoolean(if (byteBuffer.unsafeReadByte() != 0) true else false)
      case DIS.Int       => receiver.onInt(byteBuffer.unsafeReadQuadByte())
      case DIS.Long      => receiver.onLong(byteBuffer.unsafeReadOctaByte())

      case DIS.OverLong =>
        receiver.onOverLong(if (byteBuffer.unsafeReadByte() != 0) true else false, byteBuffer.unsafeReadOctaByte())

      case DIS.Float16      => receiver.onFloat16(java.lang.Float.intBitsToFloat(byteBuffer.unsafeReadQuadByte()))
      case DIS.Float        => receiver.onFloat(java.lang.Float.intBitsToFloat(byteBuffer.unsafeReadQuadByte()))
      case DIS.Double       => receiver.onDouble(java.lang.Double.longBitsToDouble(byteBuffer.unsafeReadOctaByte()))
      case DIS.NumberString => receiver.onNumberString(objBuffer.unsafeRead().asInstanceOf[String])

      case DIS.Bytes =>
        receiver.onBytes[AnyRef](objBuffer.unsafeRead())(objBuffer.unsafeRead().asInstanceOf[ByteAccess[AnyRef]])

      case DIS.BytesStart => receiver.onBytesStart()
      case DIS.String     => receiver.onString(objBuffer.unsafeRead().asInstanceOf[String])

      case DIS.Text =>
        receiver.onText[AnyRef](objBuffer.unsafeRead())(objBuffer.unsafeRead().asInstanceOf[ByteAccess[AnyRef]])

      case DIS.TextStart   => receiver.onTextStart()
      case DIS.ArrayHeader => receiver.onArrayHeader(byteBuffer.unsafeReadOctaByte())
      case DIS.ArrayStart  => receiver.onArrayStart()
      case DIS.MapHeader   => receiver.onMapHeader(byteBuffer.unsafeReadOctaByte())
      case DIS.MapStart    => receiver.onMapStart()
      case DIS.Break       => receiver.onBreak()
      case DIS.Tag         => receiver.onTag(objBuffer.unsafeRead().asInstanceOf[Tag])
      case DIS.SimpleValue => receiver.onSimpleValue(byteBuffer.unsafeReadQuadByte())
      case DIS.EndOfInput  => receiver.onEndOfInput()
    }
    1 << shift.toInt
  }

  def pullAll(receiver: Receiver): Unit = while (nonEmpty) pull(receiver)

  def dataItemValueFromEnd(offset: Int): AnyRef = objBuffer.peekFromEnd(offset)

  def appendElementFrom(r: Reader): Int = {

    // for simplicity we go for stack-based recursion here
    // if this ever becomes a problem we can upgrade to more costly heap-based recursion instead
    def pullComplex(dataItem: Int, level: Int): Unit = {

      @tailrec def pullN(remaining: Long): Unit =
        if (remaining > 0) {
          val dataItem = r.receiveInto(appendReceiver)
          if ((dataItem & DataItem.Complex) != 0) pullComplex(dataItem, level + 1)
          pullN(remaining - 1)
        }

      @tailrec def pullUntilBreak(): Unit = {
        val dataItem = r.receiveInto(appendReceiver)
        if ((dataItem & DataItem.Complex) != 0) pullComplex(dataItem, level + 1)
        if (dataItem != DataItem.Break) pullUntilBreak()
      }

      if (level < 100) {
        dataItem match {
          case DataItem.ArrayHeader => pullN(byteBuffer.peekLastOctaByte())

          case DataItem.MapHeader =>
            val elemsToPull = byteBuffer.peekLastOctaByte() << 1
            if (elemsToPull >= 0) pullN(elemsToPull)
            else sys.error("Maps with more than 2^62 elements are not supported")

          case _ => pullUntilBreak()
        }
      } else sys.error("Structures with more than 100 nesting levels are not supported") // TODO: make configurable
    }

    val first = r.receiveInto(appendReceiver)
    if ((first & DataItem.Complex) != 0) pullComplex(first, 0)
    first
  }

  def dropLastStringDataItem(): Unit = {
    byteBuffer.dropLast(1)
    objBuffer.dropLast(1)
  }

  def dropLastTextDataItem(): Unit = {
    byteBuffer.dropLast(1)
    objBuffer.dropLast(2)
  }

  def dropLastBreakDataItem(): Unit = byteBuffer.dropLast(1)

  private def ret(result: Boolean): Boolean = if (result) true else throw new ElementDeque.Overflow
}

private[borer] object ElementDeque {
  class Overflow extends RuntimeException
}