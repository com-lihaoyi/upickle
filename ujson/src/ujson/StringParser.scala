package ujson

import ujson.util.CharBuilder
import upickle.core.{ObjArrVisitor, Visitor}

/**
 * Basic in-memory string parsing.
 *
 * This is probably the simplest Parser implementation, since there is
 * no UTF-8 decoding, and the data is already fully available.
 *
 * This parser is limited to the maximum string size (~2G). Obviously
 * for large JSON documents it's better to avoid using this parser and
 * go straight from disk, to avoid having to load the whole thing into
 * memory at once. So this limit will probably not be a problem in
 * practice.
 */
private[ujson] final class StringParser[J](s: String) extends CharParser[J]{
  var line = 0
  final def column(i: Int) = i
  final def newline(i: Int) = { line += 1 }
  final def dropBufferUntil(i: Int): Unit = ()
  final def elem(i: Int): Char = upickle.core.Platform.charAt(s, i)
  final def sliceString(i: Int, j: Int): CharSequence = s.substring(i, j)

  final def atEof(i: Int) = i == s.length
  final def close() = ()
}

object StringParser extends Transformer[String]{
  def transform[T](j: String, f: Visitor[_, T]) = new StringParser(j).parse(f)
}
