package upickle.core

import java.nio.charset.StandardCharsets

import scala.annotation.switch

object RenderUtils{
  final val hexChars: Array[Int] = {
    val arr = new Array[Int](128)
    var i = 0
    while (i < 10) { arr(i + '0') = i; i += 1 }
    i = 0
    while (i < 16) { arr(i + 'a') = 10 + i; arr(i + 'A') = 10 + i; i += 1 }
    arr
  }
  def hex(i: Int): Int = hexChars(i)

  private def toHex(nibble: Int): Char = (nibble + (if (nibble >= 10) 87 else 48)).toChar

  /**
    * Attempts to write the given [[CharSequence]] into the given [[ByteBuilder]].
    *
    * Optimistically treats the characters as ASCII characters, which can be
    * directly converted to bytes and written. Only if we encounter a unicode
    * character do we fall back to the slow path of constructing a
    * [[java.lang.String]] which we UTF-8 encode before adding the to output.
    */
  final def escapeByte(unicodeCharBuilder: upickle.core.CharBuilder,
                       sb: upickle.core.ByteBuilder,
                       s: CharSequence,
                       unicode: Boolean,
                       wrapQuotes: Boolean): Unit = {

    var i = 0
    val len = s.length
    val naiveOutLen = len + (if (wrapQuotes) 2 else 0) // +2 for the start and end quotes
    sb.ensureLength(naiveOutLen)
    if (wrapQuotes) sb.appendUnsafe('"')
    while (i < len) {
      (s.charAt(i): @switch) match {
        case '"' => escapeSingleByte(sb, i, naiveOutLen, '"')
        case '\\' => escapeSingleByte(sb, i, naiveOutLen, '\\')
        case '\b' => escapeSingleByte(sb, i, naiveOutLen, 'b')
        case '\f' => escapeSingleByte(sb, i, naiveOutLen, 'f')
        case '\n' => escapeSingleByte(sb, i, naiveOutLen, 'n')
        case '\r' => escapeSingleByte(sb, i, naiveOutLen, 'r')
        case '\t' => escapeSingleByte(sb, i, naiveOutLen, 't')
        case c =>
          val notControlChar = c >= ' '
          val notUnicodeChar = c <= '~'
          if (notControlChar && notUnicodeChar) sb.append(c)
          else if (!notControlChar || (!notUnicodeChar && unicode)) {
            escapeSingleByteUnicodeEscape(sb, i, naiveOutLen, c)
          } else {
            escapeSingleByteUnicodeRaw(
              unicodeCharBuilder, sb, s, unicode, i, len, naiveOutLen, wrapQuotes
            )
            return
          }
      }
      i += 1
    }
    if (wrapQuotes) sb.appendUnsafe('"')
  }

  def escapeSingleByteUnicodeRaw(unicodeCharBuilder: CharBuilder,
                                 sb: ByteBuilder,
                                 s: CharSequence,
                                 unicode: Boolean,
                                 i: Int,
                                 len: Int,
                                 naiveOutLen: Int,
                                 wrapQuotes: Boolean) = {
    unicodeCharBuilder.reset()
    escapeChar0(i, naiveOutLen, len, unicodeCharBuilder, s, unicode, wrapQuotes)

    val bytes = unicodeCharBuilder.makeString().getBytes(StandardCharsets.UTF_8)
    sb.appendAll(bytes, bytes.length)
  }

  def escapeSingleByteUnicodeEscape(sb: ByteBuilder, i: Int, naiveOutLen: Int, c: Char) = {
    sb.ensureLength(naiveOutLen - i + 5);
    sb.appendUnsafeC('\\')
    sb.appendUnsafeC('u')
    sb.appendUnsafeC(toHex((c >> 12) & 15))
    sb.appendUnsafeC(toHex((c >> 8) & 15))
    sb.appendUnsafeC(toHex((c >> 4) & 15))
    sb.appendUnsafeC(toHex(c & 15))
  }

  def escapeSingleByte(sb: ByteBuilder, i: Int, naiveOutLen: Int, c: Char) = {
    sb.ensureLength(naiveOutLen - i + 1);
    sb.appendUnsafeC('\\');
    sb.appendUnsafeC(c)
  }

  def escapeChar(unicodeCharBuilder: upickle.core.CharBuilder,
                 sb: upickle.core.CharBuilder,
                 s: CharSequence,
                 unicode: Boolean,
                 wrapQuotes: Boolean) = {
    val len = s.length
    val naiveOutLen = len + (if (wrapQuotes) 2 else 0) // +2 for the start and end quotes
    sb.ensureLength(naiveOutLen)
    if (wrapQuotes) sb.appendUnsafe('"')
    escapeChar0(0, naiveOutLen, len, sb, s, unicode, wrapQuotes)
  }
  final def escapeChar0(i0: Int,
                        naiveOutLen: Int,
                        len: Int,
                        sb: upickle.core.CharBuilder,
                        s: CharSequence,
                        unicode: Boolean,
                        wrapQuotes: Boolean) = {
    var i = i0
    sb.ensureLength(naiveOutLen)
    while (i < len) {
      (s.charAt(i): @switch) match {
        case '"' => escapeSingleChar(sb, naiveOutLen, i, '"')
        case '\\' => escapeSingleChar(sb, naiveOutLen, i, '\\')
        case '\b' => escapeSingleChar(sb, naiveOutLen, i, 'b')
        case '\f' => escapeSingleChar(sb, naiveOutLen, i, 'f')
        case '\n' => escapeSingleChar(sb, naiveOutLen, i, 'n')
        case '\r' => escapeSingleChar(sb, naiveOutLen, i, 'r')
        case '\t' => escapeSingleChar(sb, naiveOutLen, i, 't')
        case c =>
          if (c < ' ' || (c > '~' && unicode)) {
            escapeSingleCharUnicodeEscape(naiveOutLen, sb, i, c)
          }
          else sb.append(c)
      }
      i += 1
    }
    if (wrapQuotes) sb.appendUnsafe('"')
    sb
  }

  def escapeSingleCharUnicodeEscape(naiveOutLen: Int, sb: CharBuilder, i: Int, c: Char) = {
    sb.ensureLength(naiveOutLen - i + 5);
    sb.appendUnsafeC('\\')
    sb.appendUnsafeC('u')
    sb.appendUnsafeC(toHex((c >> 12) & 15))
    sb.appendUnsafeC(toHex((c >> 8) & 15))
    sb.appendUnsafeC(toHex((c >> 4) & 15))
    sb.appendUnsafeC(toHex(c & 15))
  }

  def escapeSingleChar(sb: upickle.core.CharBuilder,
                       naiveOutLen: Int,
                       i: Int,
                       c: Char) = {
    sb.ensureLength(naiveOutLen - i + 1); sb.appendUnsafeC('\\'); sb.appendUnsafeC(c)
  }

  def intStringSize(x0: Int): Int = {
    // Taken from java.lang.Int.stringSize
    var x = x0
    var d = 1
    if (x >= 0) {
      d = 0
      x = -x
    }
    var p = -10
    var i = 1
    while (i <= 10) {
      if (x > p) return i + d
      p = 10 * p
      i += 1
    }
    10 + d
  }

  def longStringSize(x0: Long): Int = {
    // Taken from java.lang.Long.stringSize
    var x = x0
    var d = 1
    if (x >= 0) {
      d = 0
      x = -x
    }
    var p: Long = -10
    var i = 1
    while (i <= 18) {
      if (x > p) return i + d
      p = 10 * p
      i += 1
    }
    19 + d
  }
}
