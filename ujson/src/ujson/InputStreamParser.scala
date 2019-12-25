package ujson

import scala.annotation.{switch, tailrec}
import java.nio.ByteBuffer

import upickle.core.{ObjArrVisitor, Visitor, UberBuffer}
/**
  * Parser that reads in bytes from an InputStream, buffering them in memory
  * until a `reset` call discards them.
  *
  * Mostly the same as ByteArrayParser, except using an UberBuffer rather than
  * reading directly from an Array[Byte].
  *
  * Generally not meant to be used directly, but via [[ujson.Readable.fromReadable]]
  */
final class InputStreamParser[J](data: java.io.InputStream, bufferSize: Int) extends SyncParser[J] with ByteBasedParser[J] {
  protected val buffer: UberBuffer = new UberBuffer(16)
  private val streamBuffer = new Array[Byte](bufferSize)
  protected var firstIdx: Int = 0 // index in the data corresponding to the 0th element in the buffer

  protected var eof = -1
  def length: Int = firstIdx + buffer.length

  private[this] var lineState = 0
  protected[this] def line(): Int = lineState

  protected[this] final def newline(i: Int) { lineState += 1 }
  protected[this] final def column(i: Int) = i

  protected[this] final def close() {}
  protected[this] final def reset(i: Int): Int = {
    if (i > firstIdx) {
      buffer.drop(i - firstIdx)
      firstIdx = i
    }
    i
  }
  protected[this] final def checkpoint(state: Int, i: Int, stack: List[ObjArrVisitor[_, J]], path: List[Any]) {}
  protected[this] final def byte(i: Int): Byte = {
    requestUntil(i)
    buffer.apply(i - firstIdx)
  }
  protected[this] final def at(i: Int): Char = {
    requestUntil(i)
    buffer.apply(i - firstIdx).toChar
  }

  protected[this] final def at(i: Int, k: Int): CharSequence = {
    requestUntil(k)
    new String(buffer.slice(i - firstIdx, k - firstIdx))
  }

  protected[this] final def atEof(i: Int) = {
    if (i == eof) true
    else if (eof != -1 && i != eof) false
    else if (i < length) false
    else {
      readDataIntoBuffer()
      i == eof
    }
  }

  protected def requestUntil(until: Int): Unit = {
    while (this.length <= until && eof == -1) readDataIntoBuffer()
  }

  def readDataIntoBuffer() = {
    data.read(streamBuffer) match{
      case -1 => eof = length
      case n => buffer.write(streamBuffer, n)
    }
  }
}

object InputStreamParser extends Transformer[java.io.InputStream]{
  def transform[T](j: java.io.InputStream, f: Visitor[_, T]) = new InputStreamParser(j, 4096).parse(f)
}
