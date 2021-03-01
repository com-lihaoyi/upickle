package ujson

import upickle.core.{Visitor, ObjArrVisitor}


object StringParser extends Transformer[String]{
  def transform[T](j: String, f: Visitor[_, T]) = {
    BorerInputParser.transform(ujson.borer.Input.fromByteArray(j.toString.getBytes("UTF-8")), f)
  }
}
