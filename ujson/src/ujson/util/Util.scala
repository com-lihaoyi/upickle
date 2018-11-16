package ujson.util

import upickle.core.{AbortJsonProcessingException, JsonProcessingException}

object Util {
  def parseIntegralNum(s: CharSequence, decIndex: Int, expIndex: Int, index: Int) = {
    val expMul =
      if (expIndex == -1) 1
      else{
        var mult = 1
        val e = ujson.util.Util.parseLong(s, expIndex + 1, s.length())
        var i = 0
        while(i < e){
          if (mult >= Long.MaxValue / 10) throw new AbortJsonProcessingException("expected integer")
          mult = mult * 10
          i += 1
        }
        mult
      }

    val intPortion = {
      val end = if(decIndex != -1) decIndex else s.length
      ujson.util.Util.parseLong(s, 0, end) * expMul
    }

    val decPortion =
      if (decIndex == -1) 0
      else{
        val end = if(expIndex != -1) expIndex else s.length
        var value = ujson.util.Util.parseLong(s, decIndex + 1, end) * expMul
        var i = end - (decIndex + 1)
        while(i > 0) {
          value = value / 10
          i -= 1
        }
        if (s.charAt(0) == '-') -value else value
      }

    intPortion + decPortion
  }
  def parseLong(cs: CharSequence, start: Int, len: Int): Long = {

    // we store the inverse of the positive sum, to ensure we don't
    // incorrectly overflow on Long.MinValue. for positive numbers
    // this inverse sum will be inverted before being returned.
    var inverseSum: Long = 0L
    var inverseSign: Long = -1L
    var i: Int = start

    if (cs.charAt(start) == '-') {
      inverseSign = 1L
      i = 1
    }

    val size = len - i
    if (i >= len) throw new NumberFormatException(cs.toString)
    if (size > 19) throw new NumberFormatException(cs.toString)

    while (i < len) {
      val digit = cs.charAt(i).toInt - 48
      if (digit < 0 || 9 < digit) new NumberFormatException(cs.toString)
      inverseSum = inverseSum * 10L - digit
      i += 1
    }

    // detect and throw on overflow
    if (size == 19 && (inverseSum >= 0 || (inverseSum == Long.MinValue && inverseSign < 0))) {
      throw new NumberFormatException(cs.toString)
    }

    inverseSum * inverseSign
  }

  def reject(j: Int, path: List[Any]): PartialFunction[Throwable, Nothing] = {
    case e: AbortJsonProcessingException =>
      throw new JsonProcessingException(e.msg, j, -1, -1, path, e)
  }
}

