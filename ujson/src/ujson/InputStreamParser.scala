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
final class InputStreamParser[J](data: java.io.InputStream, bufferSize: Int) extends Parser[J] with ByteBasedParser[J] {
  private[this] var buffer = new Array[Byte](bufferSize)

  private[this] var firstIdx = 0
  private[this] var lastIdx = 0
  private[this] var dropped = 0

  private[this] var eof = -1

  private[this] var lineState = 0
  protected[this] def line(): Int = lineState

  protected[this] final def newline(i: Int) { lineState += 1 }
  protected[this] final def column(i: Int) = i

  protected[this] final def close() {}
  protected[this] final def dropBufferUntil(i: Int): Unit = {
    dropped = i
    i
  }
  protected[this] final def byte(i: Int): Byte = {
    requestUntil(i)
    buffer.apply(i - firstIdx)
  }
  protected[this] final def char(i: Int): Char = {
    requestUntil(i)
    buffer.apply(i - firstIdx).toChar
  }

  protected[this] final def sliceString(i: Int, k: Int): CharSequence = {
    requestUntil(k)
    new String(buffer.slice(i - firstIdx, k - firstIdx))
  }

  protected[this] final def atEof(i: Int) = {
    if (eof != -1) i == eof
    else{
      readDataIntoBuffer()
      i == eof
    }
  }

  protected def requestUntil(until: Int): Unit = {
    val requiredSize = until - firstIdx
    if (requiredSize >= buffer.size){
      var newSize = buffer.length
      while (newSize <= requiredSize) newSize *= 2

      val arr = if (newSize > buffer.length) new Array[Byte](newSize) else buffer
      System.arraycopy(buffer, dropped - firstIdx, arr, 0, lastIdx - dropped)
      firstIdx = dropped
      buffer = arr
    }

    while (lastIdx <= until && eof == -1) readDataIntoBuffer()
  }

  def readDataIntoBuffer() = {

    val bufferOffset = lastIdx - firstIdx
    data.read(buffer, bufferOffset, buffer.length - bufferOffset) match{
      case -1 => eof = lastIdx
      case n => lastIdx += n
    }
  }
}

object InputStreamParser extends Transformer[java.io.InputStream]{
  def transform[T](j: java.io.InputStream, f: Visitor[_, T]) = new InputStreamParser(j, 16 * 1024).parse(f)
}
