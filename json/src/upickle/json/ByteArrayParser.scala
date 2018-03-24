package upickle.json

import scala.annotation.{switch, tailrec}
import java.nio.ByteBuffer

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
final class ByteArrayParser[J](src: Array[Byte], start: Int = 0, limit: Int = 0) extends SyncParser[J] with ByteBasedParser[J] {
  private[this] var lineState = 0
  protected[this] def line(): Int = lineState

  protected[this] final def newline(i: Int) { lineState += 1 }
  protected[this] final def column(i: Int) = i

  protected[this] final def close() {}
  protected[this] final def reset(i: Int): Int = i
  protected[this] final def checkpoint(state: Int, i: Int, stack: List[ObjArrVisitor[_, J]]) {}
  protected[this] final def byte(i: Int): Byte = src(i + start)
  protected[this] final def at(i: Int): Char = src(i + start).toChar

  protected[this] final def at(i: Int, k: Int): CharSequence = {
    new String(src, i, k - i, utf8)
  }

  protected[this] final def atEof(i: Int) = i >= limit
}
object ByteArrayParser extends Transformer[Array[Byte]]{
  def transform[T](j: Array[Byte], f: Visitor[_, T]) = new ByteArrayParser(j).parse(f)
}