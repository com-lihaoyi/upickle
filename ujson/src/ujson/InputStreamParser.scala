package ujson

import scala.annotation.{switch, tailrec}
import java.nio.ByteBuffer

import ujson.util.ByteBuilder
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
extends ByteParser[J] with upickle.core.BufferingInputStreamParser{

  private[this] var eof = -1

  protected[this] final def close() = {}
  def loadChunk(inputArray: Array[Byte], i: Int): (Array[Byte], Int) = {
//    println(s"loadChunk($i)")
    val arr = inputArray match{
      case null => new Array[Byte](inputArrayChunkSize)
      case a => a
    }
    val n = data.readNBytes(arr, 0, arr.length)
//    println(s"n $n")
    (arr, if (n == 0) -1 else n)
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
