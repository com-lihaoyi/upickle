package ujson

import scala.annotation.{switch, tailrec}
import java.nio.ByteBuffer
import java.nio.charset.StandardCharsets

import ujson.util.ByteBuilder
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
final class ByteArrayParser[J](src: Array[Byte], start: Int = 0, limit: Int = 0) extends ByteParser[J]{

  protected[this] final def close() = {}

  def loadChunk(inputArray: Array[Byte], i: Int): (Array[Byte], Int) = {

    if(i == 0) (src, src.length)
    else (src, -1)
  }

  override def atEof(i: Int) = i >= limit
}

object ByteArrayParser extends Transformer[Array[Byte]]{
  def transform[T](j: Array[Byte], f: Visitor[_, T]) = new ByteArrayParser(j, 0, j.length).parse(f)
}
