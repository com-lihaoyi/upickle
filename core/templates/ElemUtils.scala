package upickle.core
import scala.annotation.switch
object ElemUtils{

  def appendEscapedElem(elemBuilder: ElemBuilder, c: Char, i: Int): Boolean = {
    (c: @switch) match {
      case '"' => escapeSingleElem(elemBuilder, i, '"'); true
      case '\\' => escapeSingleElem(elemBuilder, i, '\\'); true
      case '\b' => escapeSingleElem(elemBuilder, i, 'b'); true
      case '\f' => escapeSingleElem(elemBuilder, i, 'f'); true
      case '\n' => escapeSingleElem(elemBuilder, i, 'n'); true
      case '\r' => escapeSingleElem(elemBuilder, i, 'r'); true
      case '\t' => escapeSingleElem(elemBuilder, i, 't'); true
      case _ => false
    }
  }


  def escapeSingleElem(elemBuilder: ElemBuilder, i: Int, c: Char) = {
    elemBuilder.ensureLength(2);
    val length = elemBuilder.length
    val arr = elemBuilder.arr
    arr(length) = '\\'.toElem
    arr(length + 1) = c.toElem
    elemBuilder.length += 2
  }


  def escapeSingleElemUnicodeEscape(elemBuilder: ElemBuilder, i: Int, c: Char) = {
    elemBuilder.ensureLength(6)
    val arr = elemBuilder.arr
    val length = elemBuilder.length
    arr(length) = '\\'.toElem
    arr(length + 1) = 'u'.toElem
    arr(length + 2) = RenderUtils.toHex((c >> 12) & 15).toElem
    arr(length + 3) = RenderUtils.toHex((c >> 8) & 15).toElem
    arr(length + 4) = RenderUtils.toHex((c >> 4) & 15).toElem
    arr(length + 5) = RenderUtils.toHex(c & 15).toElem
    elemBuilder.length += 6
  }

  def appendSimpleStringSection(elemBuilder: ElemBuilder,
                               i0: Int,
                               len: Int,
                               s: CharSequence) = {
    elemBuilder.ensureLength(len - i0)
    val i = appendSimpleStringSection0(elemBuilder.arr, elemBuilder.length, i0, len, s)
    elemBuilder.length = elemBuilder.length + (i - i0) + 1
    i
  }

  def appendSimpleStringSectionNoUnicode(elemBuilder: ElemBuilder,
                                        i0: Int,
                                        len: Int,
                                        s: CharSequence) = {
    elemBuilder.ensureLength(len - i0)
    val i = appendSimpleStringSectionNoUnicode0(elemBuilder.arr, elemBuilder.length, i0, len, s)
    elemBuilder.length = elemBuilder.length + (i - i0) + 1
    i
  }

  private def appendSimpleStringSection0(arr: Array[Elem],
                                        arrOffset: Int,
                                        i0: Int,
                                        len: Int,
                                        s: CharSequence) = {
    var i = i0
    while (
      if (i >= len) false
      else {
        val c2 = s.charAt(i)
        if (c2 < ' ' | c2 == '"' | c2 == '\\') false
        else {
          arr(arrOffset + i - i0) = c2.toElem
          i += 1
          true
        }
      }
    ) ()

    i - 1
  }

  private def appendSimpleStringSectionNoUnicode0(arr: Array[Elem],
                                                 arrOffset: Int,
                                                 i0: Int,
                                                 len: Int,
                                                 s: CharSequence) = {
    var i = i0
    while (
      if (i >= len) false
      else {
        val c2 = s.charAt(i)
        if (c2 < ' ' || c2 > 127 || c2 == '"' || c2 == '\\') false
        else {
          arr(arrOffset + i - i0) = c2.toElem
          i += 1
          true
        }
      }
    ) ()

    i - 1
  }


  def parseIntegralNum(arr: Array[Elem], arrOffset: Int, arrLength: Int, decIndex: Int, expIndex: Int) = {
    val expMul =
      if (expIndex == -1) 1
      else {
        var mult = 1
        val e = parseLong(arr, arrOffset + expIndex + 1, arrOffset + arrLength)
        var i = 0
        while (i < e) {
          if (mult >= Long.MaxValue / 10) throw new Abort("expected integer")
          mult = mult * 10
          i += 1
        }
        mult
      }

    val intPortion = {
      val end =
        if (decIndex != -1) decIndex
        else if (expIndex != -1) expIndex
        else arrLength

      parseLong(arr, arrOffset, arrOffset + end) * expMul
    }

    val decPortion =
      if (decIndex == -1) 0
      else {
        val end = if (expIndex != -1) expIndex else arrLength
        var value = parseLong(arr, arrOffset + decIndex + 1, arrOffset + end) * expMul
        var i = end - (decIndex + 1)
        while (i > 0) {
          value = value / 10
          i -= 1
        }
        if (arr(arrOffset) == '-') -value else value
      }

    intPortion + decPortion
  }

  def parseLong(cs0: Array[Elem], start0: Int, end0: Int): Long = {
    if ((start0 | end0 | end0 - start0 | (cs0.length - end0)) < 0) throw new IndexOutOfBoundsException

    // If we do not copy the data from `cs0` into our own local array before
    // parsing it, we take a significant performance penalty in the
    // `integers Read` benchmarks, but *only* when run together with the rest
    // of the benchmarks! When `integers Read` isrun alone, this does not
    // happen, and is presumably something to do with the JIT compiler.

    // Since any real world use case would exercise all sorts of code paths,
    // it would more closely resemble the "all benchmarks together" case
    // rather, and so we leave this copy in-place to optimize performance
    // for that scenario.
    val cs = new Array[Elem](end0 - start0)
    System.arraycopy(cs0, start0, cs, 0, end0 - start0)

    // we store the inverse of the positive sum, to ensure we don't
    // incorrectly overflow on Long.MinValue. for positive numbers
    // this inverse sum will be inverted before being returned.
    var inverseSum: Long = 0L
    var inverseSign: Long = -1L
    var i: Int = 0
    val end = end0 - start0

    if (cs(0) == '-') {
      inverseSign = 1L
      i += 1
    }

    val size = end - i
    if (size <= 0 || size > 19) throw new NumberFormatException(new String(cs))

    while (i < end) {
      val digit = cs(i).toInt - 48
      if (digit < 0 || 9 < digit) throw new NumberFormatException(new String(cs))
      inverseSum = inverseSum * 10L - digit
      i += 1
    }

    // detect and throw on overflow
    if (size == 19 && (inverseSum >= 0 || (inverseSum == Long.MinValue && inverseSign < 0))) {
      throw new NumberFormatException(new String(cs))
    }

    inverseSum * inverseSign
  }
}