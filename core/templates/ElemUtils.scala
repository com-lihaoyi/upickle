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

}