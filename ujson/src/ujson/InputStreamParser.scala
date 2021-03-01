package ujson

import scala.annotation.{switch, tailrec}
import java.nio.ByteBuffer

import upickle.core.{ObjArrVisitor, Visitor}

object InputStreamParser extends Transformer[java.io.InputStream]{
  def transform[T](j: java.io.InputStream, f: Visitor[_, T]) = {
    BorerInputParser.transform(ujson.borer.Input.fromInputStream(j), f)
  }
}
