package ujson

import ujson.util.CharBuilder
import upickle.core.{ObjArrVisitor, Visitor}
/**
 * Lazy character sequence parsing.
 *
 * This is similar to StringParser, but acts on character sequences.
 */
private[ujson] final class CharSequenceParser[J](cs: CharSequence) extends CharParser[J]{
  def readDataIntoBuffer(buffer: Array[Char], bufferOffset: Int) = {
    if(buffer == null) (cs.toString.toCharArray, false, cs.length)
    else (buffer, true, -1)
  }
  override def atEof(i: Int) = i >= cs.length
  final def close() = ()
}

object CharSequenceParser extends Transformer[CharSequence]{
  def transform[T](j: CharSequence, f: Visitor[_, T]) = new CharSequenceParser(j).parse(f)
}
