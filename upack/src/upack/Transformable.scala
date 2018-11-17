package upack

import upickle.core.Visitor

abstract class Transformable {
  def transform[T](f: Visitor[_, T]): T
}

object Transformable {
  implicit def fromByteArray(s: Array[Byte]) = new Transformable{
    def transform[T](f: Visitor[_, T]): T = new MsgPackReader(0, s).parse(f)
  }
}