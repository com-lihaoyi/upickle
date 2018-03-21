package upickle.jawn

import java.nio.ByteBuffer
import java.nio.channels.ReadableByteChannel

abstract class Transformable {
  def transform[T](f: upickle.jawn.Visitor[_, T]): T
}
object Transformable{
  class WalkerTransformable[T](t: T, w: Transformer[T]) extends Transformable{
    def transform[T](f: upickle.jawn.Visitor[_, T]): T = {
      w.transform(t, f)
    }
  }
  implicit def fromString(s: String) = new WalkerTransformable(s, StringParser)
  implicit def fromCharSequence(s: CharSequence) = new WalkerTransformable(s, CharSequenceParser)
  implicit def fromChannel(s: ReadableByteChannel) = new WalkerTransformable(s, ChannelParser)
  implicit def fromPath(s: java.nio.file.Path) = new WalkerTransformable(s, PathParser)
  implicit def fromFile(s: java.io.File) = new WalkerTransformable(s, FileParser)
  implicit def fromByteBuffer(s: ByteBuffer) = new WalkerTransformable(s, ByteBufferParser)
  implicit def fromByteArray(s: Array[Byte]) = new WalkerTransformable(s, ByteArrayParser)
}