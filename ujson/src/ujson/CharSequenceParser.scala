package ujson

import ujson.util.CharBuilder
import upickle.core.{ObjArrVisitor, Visitor}
/**
 * Lazy character sequence parsing.
 *
 * This is similar to StringParser, but acts on character sequences.
 */
private[ujson] final class CharSequenceParser[J](cs: CharSequence) extends CharParser[J]{
  var line = 0
  final def column(i: Int) = i
  final def newline(i: Int) = { line += 1 }
  final def dropBufferUntil(i: Int): Unit = ()
  final def elem(i: Int): Char = upickle.core.Platform.charAt(cs, i)
  final def sliceString(i: Int, j: Int): CharSequence = cs.subSequence(i, j)
  final def atEof(i: Int) = i == cs.length
  final def close() = ()
}

object CharSequenceParser extends Transformer[CharSequence]{
  def transform[T](j: CharSequence, f: Visitor[_, T]) = new CharSequenceParser(j).parse(f)
}
