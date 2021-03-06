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
extends ByteParser[J] with upickle.core.BufferingInputStreamParser{
  override def defaultStartBufferSize: Int = 1024
  protected[this] final def close() = {}

  var knownEof = Int.MaxValue
  def atEof(i: Int): Boolean = {
    if (knownEof < i) true
    else {
      val res = requestUntil(i)
      if(res) knownEof = i
      res
    }
  }
}

object InputStreamParser extends Transformer[java.io.InputStream]{
  def transform[T](j: java.io.InputStream, f: Visitor[_, T]) = {
    val p = new InputStreamParser[T](j)
    p.parse(f)
  }
}
