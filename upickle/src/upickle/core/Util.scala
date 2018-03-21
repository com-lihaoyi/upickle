package upickle
package core

import upickle.jawn.ObjArrVisitor

object Util {
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
    if (cs.charAt(i) == '0' && size > 1) throw new NumberFormatException(cs.toString)

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
}

