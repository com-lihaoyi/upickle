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
  final def dropBufferUntil(i: Int): Unit = ()
  def loadChunk(inputArray: Array[Char], i: Int) = {
//    println(s"loadChunk($i)")
    if (i == s.length()) (null, -1)
    else {
      val arr = inputArray match {
        case null => new Array[Char](inputArrayChunkSize)
        case a => a
      }
      var j = i
      val max = math.min(i + inputArrayChunkSize, s.length)
      while (j < max) {
        arr(j - i) = s.charAt(j)
        j += 1
      }
      (arr, max - i)
    }
  }
  final def sliceString(i: Int, j: Int): CharSequence = s.substring(i, j)

  final def atEof(i: Int) = i == s.length
  final def close() = ()
}

object StringParser extends Transformer[String]{
  def transform[T](j: String, f: Visitor[_, T]) = new StringParser(j).parse(f)
}
