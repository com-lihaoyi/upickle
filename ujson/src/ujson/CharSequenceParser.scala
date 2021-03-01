package ujson

import upickle.core.{Visitor, ObjArrVisitor}

object CharSequenceParser extends Transformer[CharSequence]{
  def transform[T](j: CharSequence, f: Visitor[_, T]) = {
    BorerInputParser.transform(ujson.borer.Input.fromByteArray(j.toString.getBytes("UTF-8")), f)
  }
}
