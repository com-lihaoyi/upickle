package ujson

import java.nio.ByteBuffer
import java.nio.channels.ReadableByteChannel
import upickle.core.{Visitor, ObjArrVisitor}
trait Readable {
  def transform[T](f: Visitor[_, T]): T
}

object Readable {
  case class fromTransformer[T](t: T, w: Transformer[T]) extends Readable{
    def transform[T](f: Visitor[_, T]): T = {
      w.transform(t, f)
    }
  }
  implicit def fromString(s: String) = new fromTransformer(s, StringParser)
  implicit def fromCharSequence(s: CharSequence) = new fromTransformer(s, CharSequenceParser)
  implicit def fromChannel(s: ReadableByteChannel) = new fromTransformer(s, ChannelParser)
  implicit def fromPath(s: java.nio.file.Path) = new fromTransformer(s, PathParser)
  implicit def fromFile(s: java.io.File) = new fromTransformer(s, FileParser)
  implicit def fromByteBuffer(s: ByteBuffer) = new fromTransformer(s, ByteBufferParser)
  implicit def fromByteArray(s: Array[Byte]) = new fromTransformer(s, ByteArrayParser)
}
