package ujson.borer

import output._

import scala.annotation.tailrec

/**
  * Abstraction over serialization output.
  *
  * The implementation can be either mutable or immutable.
  */
trait Output { outer =>
  type Self <: Output { type Self <: outer.Self }
  type Result <: AnyRef

  def writeByte(byte: Byte): Self
  def writeBytes(a: Byte, b: Byte): Self
  def writeBytes(a: Byte, b: Byte, c: Byte): Self
  def writeBytes(a: Byte, b: Byte, c: Byte, d: Byte): Self

  def writeShort(value: Short): Self =
    writeBytes((value >> 8).toByte, value.toByte)

  def writeInt(value: Int): Self =
    writeBytes((value >> 24).toByte, (value >> 16).toByte, (value >> 8).toByte, value.toByte)

  def writeLong(value: Long): Self =
    writeInt((value >> 32).toInt).writeInt(value.toInt)

  def writeBytes[Bytes: ByteAccess](bytes: Bytes): Self

  def result(): Result
}

object Output extends ToByteArrayOutput with ToByteBufferOutput with ToFileOutput with ToOutputStreamOutput {

  //#provider
  /**
    * Responsible for providing an Output that produces instances of [[T]].
    */
  trait ToTypeProvider[T] {
    type Out <: Output { type Result = T }
    def apply(bufferSize: Int, allowBufferCaching: Boolean): Out
  }

  /**
    * Responsible for providing an Output that outputs into the given value [[T]].
    */
  trait ToValueProvider[T] {
    type Out <: Output { type Result = T }
    def apply(value: T, bufferSize: Int, allowBufferCaching: Boolean): Out
  }
  //#provider

  implicit final class OutputOps(val underlying: Output) extends AnyVal {
    @inline def writeAsByte(i: Int): underlying.Self = underlying.writeByte(i.toByte)

    @inline def writeAsByte(c: Char): underlying.Self           = underlying.writeByte(c.toByte)
    @inline def writeAsBytes(a: Char, b: Char): underlying.Self = underlying.writeBytes(a.toByte, b.toByte)

    @inline def writeAsBytes(a: Char, b: Char, c: Char): underlying.Self =
      underlying.writeBytes(a.toByte, b.toByte, c.toByte)

    @inline def writeAsBytes(a: Char, b: Char, c: Char, d: Char): underlying.Self =
      underlying.writeBytes(a.toByte, b.toByte, c.toByte, d.toByte)

    def writeStringAsAsciiBytes(s: String): underlying.Self = {
      @tailrec def rec(out: underlying.Self, ix: Int): underlying.Self =
        s.length - ix match {
          case 0 => out
          case 1 => writeAsByte(s.charAt(ix))
          case 2 => writeAsBytes(s.charAt(ix), s.charAt(ix + 1))
          case 3 => writeAsBytes(s.charAt(ix), s.charAt(ix + 1), s.charAt(ix + 2))
          case _ => rec(writeAsBytes(s.charAt(ix), s.charAt(ix + 1), s.charAt(ix + 2), s.charAt(ix + 3)), ix + 4)
        }
      rec(underlying.asInstanceOf[underlying.Self], 0)
    }
  }
}