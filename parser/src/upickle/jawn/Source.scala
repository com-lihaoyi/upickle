package upickle.jawn

import java.nio.ByteBuffer
import java.nio.channels.ReadableByteChannel

abstract class Source {
  def transform[T](f: upickle.jawn.Visitor[_, T]): T
}
object Source{
  class WalkerSource[T](t: T, w: Transformer[T]) extends Source{
    def transform[T](f: upickle.jawn.Visitor[_, T]): T = {
      w.transform(t, f)
    }
  }
  implicit def fromString(s: String) = new WalkerSource(s, StringParser)
  implicit def fromCharSequence(s: CharSequence) = new WalkerSource(s, CharSequenceParser)
  implicit def fromChannel(s: ReadableByteChannel) = new WalkerSource(s, ChannelParser)
  implicit def fromPath(s: java.nio.file.Path) = new WalkerSource(s, PathParser)
  implicit def fromFile(s: java.io.File) = new WalkerSource(s, FileParser)
  implicit def fromByteBuffer(s: ByteBuffer) = new WalkerSource(s, ByteBufferParser)
  implicit def fromByteArray(s: Array[Byte]) = new WalkerSource(s, ByteArrayParser)
}