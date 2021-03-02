package ujson

import ujson.util.FastByteArrayOutputStream

import scala.annotation.{switch, tailrec}

/**
 * Trait used when the data to be parsed is in UTF-8.
 *
 * This parser has to translate input bytes to Chars and Strings. It
 * provides a byte() method to access individual bytes, and also
 * parser strings from bytes.
 *
 * Its parseString() implementation has two cases. In the first case
 * (the hot path) the string has no escape sequences and we can just
 * UTF-8 decode the entire set of bytes. In the second case, it goes
 * to some trouble to be sure to de-escape correctly given that the
 * input data is UTF-8.
 */
trait ByteBasedParser[J] extends Parser[J] {
  protected[this] def byte(i: Int): Byte
  var boas = new FastByteArrayOutputStream()
  /**
   * See if the string has any escape sequences. If not, return the end of the
   * string. If so, bail out and return -1.
   *
   * This method expects the data to be in UTF-8 and accesses it as bytes. Thus
   * we can just ignore any bytes with the highest bit set.
   */
  protected[this] final def parseStringSimple(i: Int): Int = {
    var j = i
    var c: Int = byte(j) & 0xff
    while (c != '"') {
      if (c >= 0 && c  < ' ') die(j, s"control char ($c) in string")
      if (c == '\\') return -j - 1
      j += 1
      boas.write(c.toByte)
      c = byte(j) & 0xff
    }
    j + 1
  }

  protected[this] final def parseStringComplex(i0: Int) = {

    var i = i0

    var c: Int = byte(i) & 0xff
    while (c != '"') {
      if (c == '\\') {
        (byte(i + 1): @switch) match {
          case 'b' => { boas.write('\b'); i += 2 }
          case 'f' => { boas.write('\f'); i += 2 }
          case 'n' => { boas.write('\n'); i += 2 }
          case 'r' => { boas.write('\r'); i += 2 }
          case 't' => { boas.write('\t'); i += 2 }

          case '"' => { boas.write('"'); i += 2 }
          case '/' => { boas.write('/'); i += 2 }
          case '\\' => { boas.write('\\'); i += 2 }

          // if there's a problem then descape will explode
          case 'u' =>
            val char = descape(sliceString(i + 2, i + 6))
            boas.write((char & 0xff).toByte)
            boas.write((char >> 8).toByte)
            i += 6

          case c => die(i, s"invalid escape sequence (\\${c.toChar})")
        }
      }
      else if (c >= 0 && c < ' ') die(i, s"control char ($c) in string")
      else {
        boas.write(c.toByte)
        i = i + 1
      }
      c = byte(i) & 0xff
    }

    (boas.makeString(), i + 1)
  }
  /**
   * Parse the string according to JSON rules, and add to the given context.
   *
   * This method expects the data to be in UTF-8 and accesses it as bytes.
   */
  protected[this] final def parseString(i: Int, key: Boolean): (CharSequence, Int) = {
    boas.reset()
    val k = parseStringSimple(i + 1)
    if (k >= 0) (boas.makeString(), k)
    else parseStringComplex(-k - 1)
  }
}
