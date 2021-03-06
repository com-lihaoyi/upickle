package ujson
import upickle.core.{ObjArrVisitor, Visitor}

import scala.annotation.{switch, tailrec}
import java.nio.ByteBuffer
import java.nio.charset.StandardCharsets

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
final class ByteBufferParser[J](src: ByteBuffer) extends ByteParser[J]{
  private[this] final val start = src.position()
  private[this] final val limit = src.limit() - start


  protected[this] final def close() = { src.position(src.limit) }
  def readDataIntoBuffer(buffer: Array[Byte], bufferOffset: Int) = {
//    assert(i == 0)
//    src.
    ???
  }


  override def atEof(i: Int) = i >= limit
}

object ByteBufferParser extends Transformer[ByteBuffer]{
  def transform[T](j: ByteBuffer, f: Visitor[_, T]) = new ByteBufferParser(j).parse(f)
}
