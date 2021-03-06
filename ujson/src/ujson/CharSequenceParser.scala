package ujson

import ujson.util.CharBuilder
import upickle.core.{ObjArrVisitor, Visitor}
/**
 * Lazy character sequence parsing.
 *
 * This is similar to StringParser, but acts on character sequences.
 */
private[ujson] final class CharSequenceParser[J](cs: CharSequence) extends CharParser[J]{

  final def dropBufferUntil(i: Int): Unit = ()
  def loadChunk(inputArray: Array[Char], i: Int) = {
    if (i == cs.length()) (null, -1)
    else {
      val arr = inputArray match {
        case null => new Array[Char](inputArrayChunkSize)
        case a => a
      }
      var j = i
      val max = math.min(i + inputArrayChunkSize, cs.length)
      while (j < max) {
        arr(j - i) = cs.charAt(j)
        j += 1
      }
      (arr, max - i)
    }
  }
  final def sliceString(i: Int, j: Int): CharSequence = cs.subSequence(i, j)
  final def atEof(i: Int) = i == cs.length
  final def close() = ()
}

object CharSequenceParser extends Transformer[CharSequence]{
  def transform[T](j: CharSequence, f: Visitor[_, T]) = new CharSequenceParser(j).parse(f)
}
