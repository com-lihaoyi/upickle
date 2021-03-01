package ujson
import upickle.core.{Visitor, ObjArrVisitor}
import scala.annotation.{switch, tailrec}
import java.nio.ByteBuffer


object ByteBufferParser extends Transformer[ByteBuffer]{
  def transform[T](j: ByteBuffer, f: Visitor[_, T]) = {
    BorerInputParser.transform(ujson.borer.Input.fromByteBuffer(j), f)
  }
}
