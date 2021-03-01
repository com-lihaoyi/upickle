package ujson


import upickle.core.Visitor

object ByteArrayParser extends Transformer[Array[Byte]]{

  def transform[T](j: Array[Byte], f: Visitor[_, T]) = {
    BorerInputParser.transform(ujson.borer.Input.fromByteArray(j), f)
  }
}
