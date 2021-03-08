package upack

import upickle.core.Visitor

trait Readable {
  def transform[T](f: Visitor[_, T]): T
}

object Readable extends ReadableLowPri {
  implicit def fromByteArray(s: Array[Byte]): Readable = new Readable{
    def transform[T](f: Visitor[_, T]): T = new MsgPackReader(s).parse(f)
  }
}

trait ReadableLowPri{
  implicit def fromReadable[T](s: T)(implicit conv: T => geny.Readable): Readable = new Readable{
    def transform[T](f: Visitor[_, T]): T = conv(s).readBytesThrough(
      new InputStreamMsgPackReader(
        _,
        upickle.core.BufferingInputStreamParser.defaultMinBufferStartSize,
        upickle.core.BufferingInputStreamParser.defaultMaxBufferStartSize
      ).parse(f)
    )
  }
}
