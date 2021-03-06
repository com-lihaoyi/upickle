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
final class InputStreamParser[J](val data: java.io.InputStream)
extends ByteParser[J] {

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
}

object InputStreamParser extends Transformer[java.io.InputStream]{
  def transform[T](j: java.io.InputStream, f: Visitor[_, T]) = {
    val p = new InputStreamParser[T](j)
    p.parse(f)
  }
}
