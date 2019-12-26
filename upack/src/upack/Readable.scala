package upack

import upickle.core.Visitor

trait Readable {
  def transform[T](f: Visitor[_, T]): T
}

object Readable {
  implicit def fromByteArray(s: Array[Byte]) = new Readable{
    def transform[T](f: Visitor[_, T]): T = new MsgPackReader(0, s).parse(f)
  }
  implicit def fromReadable(s: geny.Readable) = new Readable{
    def transform[T](f: Visitor[_, T]): T = {
      s.readBytesThrough(
        new InputStreamMsgPackReader(
          _,
          upickle.core.BufferingInputStreamParser.defaultMinBufferStartSize,
          upickle.core.BufferingInputStreamParser.defaultMaxBufferStartSize
        ).parse(f)
      )
    }
  }
}