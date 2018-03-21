package upickle.jawn

import java.nio.ByteBuffer
import java.nio.channels.ReadableByteChannel

abstract class Source {
  def apply[T](f: upickle.jawn.Visitor[_, T]): T
}
object Source{
  class WalkerSource[T](t: T, w: Walker[T]) extends Source{
    def apply[T](f: upickle.jawn.Visitor[_, T]): T = {
      w.visit(t, f)
    }
  }
  implicit def fromString(s: String) = new WalkerSource(s, StringParser)
  implicit def fromCharSequence(s: CharSequence) = new WalkerSource(s, CharSequenceParser)
  implicit def fromChannel(s: ReadableByteChannel) = new WalkerSource(s, ChannelParser)
  implicit def fromByteBuffer(s: ByteBuffer) = new WalkerSource(s, ByteBufferParser)
  implicit def fromByteArray(s: Array[Byte]) = new WalkerSource(s, ByteArrayParser)

  implicit class fromPath(t: java.nio.file.Path) extends Source{
    def apply[T](f: upickle.jawn.Visitor[_, T]): T = {
      val channel = java.nio.file.Files.newByteChannel(t)
      try ChannelParser.visit(channel, f)
      finally channel.close()
    }
  }
  implicit class fromFile(t: java.io.File) extends Source{
    def apply[T](f: upickle.jawn.Visitor[_, T]): T = {
      val channel = java.nio.file.Files.newByteChannel(t.toPath)
      try ChannelParser.visit(channel, f)
      finally channel.close()
    }
  }
}