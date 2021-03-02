package ujson

import scala.annotation.{switch, tailrec}
import java.nio.ByteBuffer
import upickle.core.{Visitor, ObjArrVisitor}
/**
  * Basic ByteBuffer parser.
  *
  * This assumes that the provided ByteBuffer is ready to be read. The
  * user is responsible for any necessary flipping/resetting of the
  * ByteBuffer before parsing.
  *
  * The parser makes absolute calls to the ByteBuffer, which will not
  * update its own mutable position fields.
  */
final class ByteArrayParser[J](src: Array[Byte], start: Int = 0, limit: Int = 0) extends Parser[J] with ByteBasedParser[J] {
  private[this] var lineState = 0
  protected[this] def line: Int = lineState

  protected[this] final def newline(i: Int) = { lineState += 1 }
  protected[this] final def column(i: Int) = i

  protected[this] final def close() = {}
  protected[this] final def dropBufferUntil(i: Int): Unit = ()
  protected[this] final def byte(i: Int): Byte = upickle.core.Platform.byteAt(src, i + start)
  protected[this] final def char(i: Int): Char = upickle.core.Platform.byteAt(src, i + start).toChar

  protected[this] final def sliceString(i: Int, k: Int): CharSequence = {
    new String(src, i, k - i, utf8)
  }
  protected[this] final def sliceStringInto(i: Int, k: Int, builder: ujson.util.CharBuilder): Unit = {
    builder.extend(new String(src, i, k - i, utf8))
  }

  protected[this] final def atEof(i: Int) = i >= limit
}

object ByteArrayParser extends Transformer[Array[Byte]]{
  def transform[T](j: Array[Byte], f: Visitor[_, T]) = new ByteArrayParser(j, 0, j.length).parse(f)
}
