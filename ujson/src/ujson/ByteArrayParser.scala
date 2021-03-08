package ujson

import scala.annotation.{switch, tailrec}
import java.nio.ByteBuffer
import java.nio.charset.StandardCharsets

import upickle.core.{ObjArrVisitor, Visitor}
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
final class ByteArrayParser[J](src: Array[Byte]) extends ByteParser[J]{

  val srcLength = src.length
  protected[this] final def close() = {}

  // Make sure we never call this method, since it will mutate the original array,
  // and it should not be necessary to call it if our implementation is correct.
  override def growBuffer(until: Int): Unit = ???

  def readDataIntoBuffer(buffer: Array[Byte], bufferOffset: Int) = {
    if(buffer == null) (src, false, srcLength)
    else (src, true, -1)
  }
  override def atEof(i: Int) = i >= srcLength
}

object ByteArrayParser extends Transformer[Array[Byte]]{
  def transform[T](j: Array[Byte], f: Visitor[_, T]) = new ByteArrayParser(j).parse(f)
}
