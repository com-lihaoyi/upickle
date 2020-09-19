package ujson

import java.nio.ByteBuffer
import java.nio.channels.ReadableByteChannel
import upickle.core.{Visitor, ObjArrVisitor}
trait Readable {
  def transform[T](f: Visitor[_, T]): T
}

object Readable extends ReadableLowPri{
  case class fromTransformer[T](t: T, w: Transformer[T]) extends Readable{
    def transform[T](f: Visitor[_, T]): T = {
      w.transform(t, f)
    }
  }
  implicit def fromString(s: String): fromTransformer[String] = new fromTransformer(s, StringParser)
  implicit def fromCharSequence(s: CharSequence): fromTransformer[CharSequence] = new fromTransformer(s, CharSequenceParser)
  implicit def fromPath(s: java.nio.file.Path): Readable = new Readable {
    override def transform[T](f: Visitor[_, T]) = {
      val inputStream = java.nio.file.Files.newInputStream(s)
      try InputStreamParser.transform(inputStream, f)
      finally inputStream.close()
    }
  }
  implicit def fromFile(s: java.io.File): Readable = fromPath(s.toPath)
  implicit def fromByteBuffer(s: ByteBuffer): fromTransformer[ByteBuffer] = new fromTransformer(s, ByteBufferParser)
  implicit def fromByteArray(s: Array[Byte]): fromTransformer[Array[Byte]] = new fromTransformer(s, ByteArrayParser)
}

trait ReadableLowPri{
  implicit def fromReadable[T](s: T)(implicit conv: T => geny.Readable): Readable = new Readable{
    def transform[T](f: Visitor[_, T]): T = conv(s).readBytesThrough(InputStreamParser.transform(_, f))
  }
}
