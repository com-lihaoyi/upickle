package ujson

import scala.annotation.{switch, tailrec}
import java.nio.ByteBuffer

import upickle.core.{ObjArrVisitor, Visitor}
/**
  * Parser that reads in bytes from an InputStream, buffering them in memory
  * until a `reset` call discards them.
  *
  * Mostly the same as ByteArrayParser, except using an UberBuffer rather than
  * reading directly from an Array[Byte].
  *
  * Generally not meant to be used directly, but via [[ujson.Readable.fromReadable]]
  */
final class InputStreamParser[J](val data: java.io.InputStream,
                                 val minStartBufferSize: Int,
                                 val maxStartBufferSize: Int)
extends Parser[J] with ByteBasedParser[J] with upickle.core.BufferingInputStreamParser{

  private[this] var eof = -1

  private[this] var lineState = 0
  protected[this] def line: Int = lineState

  protected[this] final def newline(i: Int) = { lineState += 1 }
  protected[this] final def column(i: Int) = i

  protected[this] final def close() = {}
  protected[this] final def char(i: Int): Char = {
    byte(i).toChar
  }

  protected[this] final def atEof(i: Int) = {
    if (eof != -1) i == eof
    else{
      val done = readDataIntoBuffer()
      if (done) eof = getLastIdx
      i == eof
    }
  }

  override protected def requestUntil(until: Int): Boolean = {
    val done = super.requestUntil(until)
    if (done) eof = getLastIdx
    done
  }

}

object InputStreamParser extends Transformer[java.io.InputStream]{
  def transform[T](j: java.io.InputStream, f: Visitor[_, T]) = {
    val p = new InputStreamParser[T](
      j,
      upickle.core.BufferingInputStreamParser.defaultMinBufferStartSize,
      upickle.core.BufferingInputStreamParser.defaultMaxBufferStartSize
    )
    p.parse(f)
  }
}
