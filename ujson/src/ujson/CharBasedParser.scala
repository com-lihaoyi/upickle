package ujson

import scala.annotation.switch

/**
 * Trait used when the data to be parsed is in UTF-16.
 *
 * This parser provides parseString(). Like ByteBasedParser it has
 * fast/slow paths for string parsing depending on whether any escapes
 * are present.
 *
 * It is simpler than ByteBasedParser.
 */
trait CharBasedParser[J] extends Parser[J] {

  private[this] final val charBuilder = new ujson.util.CharBuilder()

  /**
   * See if the string has any escape sequences. If not, return the
   * end of the string. If so, bail out and return -1.
   *
   * This method expects the data to be in UTF-16 and accesses it as
   * chars.
   */
  protected[this] final def parseStringSimple(i: Int): Int = {
    var j = i
    var c = char(j)
    while (c != '"') {
      if (c < ' ') die(j, s"control char (${c.toInt}) in string")
      if (c == '\\') return -1 - j
      j += 1
      charBuilder.append(c)
      c = char(j)
    }
    j + 1
  }

  /**
   * Parse a string that is known to have escape sequences.
   */
  protected[this] final def parseStringComplex(i0: Int): (CharSequence, Int) = {

    var i = i0
    var c = char(i)
    while (c != '"') {
      if (c < ' ') {
        die(i, s"control char (${c.toInt}) in string")
      } else if (c == '\\') {
        (char(i + 1): @switch) match {
          case 'b' => { charBuilder.append('\b'); i += 2 }
          case 'f' => { charBuilder.append('\f'); i += 2 }
          case 'n' => { charBuilder.append('\n'); i += 2 }
          case 'r' => { charBuilder.append('\r'); i += 2 }
          case 't' => { charBuilder.append('\t'); i += 2 }

          case '"' => { charBuilder.append('"'); i += 2 }
          case '/' => { charBuilder.append('/'); i += 2 }
          case '\\' => { charBuilder.append('\\'); i += 2 }

          // if there's a problem then descape will explode
          case 'u' => { charBuilder.append(descape(sliceString(i + 2, i + 6))); i += 6 }

          case c => die(i + 1, s"illegal escape sequence after \\")
        }
      } else {
        // this case is for "normal" code points that are just one Char.
        //
        // we don't have to worry about surrogate pairs, since those
        // will all be in the ranges D800–DBFF (high surrogates) or
        // DC00–DFFF (low surrogates).
        charBuilder.append(c)
        i += 1
      }
      c = char(i)
    }

    (charBuilder.makeString, i + 1)
  }

  /**
   * Parse the string according to JSON rules, and add to the given
   * context.
   *
   * This method expects the data to be in UTF-16, and access it as
   * Char. It performs the correct checks to make sure that we don't
   * interpret a multi-char code point incorrectly.
   */
  protected[this] final def parseString(i: Int,  key: Boolean): (CharSequence, Int) = {
    charBuilder.reset()
    val k = parseStringSimple(i + 1)
    if (k >= 0) (charBuilder.makeString, k)
    else parseStringComplex(-k - 1)
  }
}
