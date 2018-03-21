package upickle.jawn

import java.nio.ByteBuffer
import java.nio.channels.ReadableByteChannel

abstract class Source {
  def apply[T](f: upickle.jawn.RawFacade[_, T]): T
}
object Source{
  class WalkerSource[T](t: T, w: Walker[T]) extends Source{
    def apply[T](f: upickle.jawn.RawFacade[_, T]): T = {
      w.visit(t, f)
    }
  }
  implicit def stringSource(s: String) = new WalkerSource(s, StringParser)
  implicit def charSeqSource(s: CharSequence) = new WalkerSource(s, CharSequenceParser)
  implicit def channelSource(s: ReadableByteChannel) = new WalkerSource(s, ChannelParser)
  implicit def byteBufferSource(s: ByteBuffer) = new WalkerSource(s, ByteBufferParser)
  implicit def byteArraySource(s: Array[Byte]) = new WalkerSource(s, ByteArrayParser)
}