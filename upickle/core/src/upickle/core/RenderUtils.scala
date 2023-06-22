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

  def toHex(nibble: Int): Char = (nibble + (if (nibble >= 10) 87 else 48)).toChar

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
                       escapeUnicode: Boolean,
                       wrapQuotes: Boolean): Unit = {

    val len = s.length
    if (wrapQuotes) sb.append('"')
    var i = 0
    // Split the loop based on `escapeUnicode`,
    // to avoid repeating the check within the hot loop
    if (escapeUnicode){
      while (i < len) {
        val c = s.charAt(i)
        if (!ByteUtils.appendEscapedByte(sb, c, i)) {
          if (c < ' ' || c > 127) { ByteUtils.escapeSingleByteUnicodeEscape(sb, i, c) }
          else { i = ByteUtils.appendSimpleStringSectionNoUnicode(sb, i, len, s) }
        }
        i += 1
      }
    }else{
      while (i < len) {
        val c = s.charAt(i)
        if (!ByteUtils.appendEscapedByte(sb, c, i)) {
          if (c < ' ') { ByteUtils.escapeSingleByteUnicodeEscape(sb, i, c) }
          else if (c <= '~') { i = ByteUtils.appendSimpleStringSectionNoUnicode(sb, i, len, s) }
          else {
            // If we hit a unicode character, fall back to `escapeSingleByteUnicodeRaw`
            // to handle the remainder of the string. Somehow this benchmarks almost
            // twice as fast as trying to handle the unicode characters directly in this
            // method, for unclear reasons (???)
            escapeSingleByteUnicodeRaw(
              unicodeCharBuilder, sb, s, false, i, len, wrapQuotes
            )
            return
          }
        }
        i += 1
      }
    }

    if (wrapQuotes) sb.append('"')
  }

  @deprecated("Not used, kept for binary compatibility")
  def escapeSingleByteUnicodeRaw(unicodeCharBuilder: CharBuilder,
                                 sb: ByteBuilder,
                                 s: CharSequence,
                                 escapeUnicode: Boolean,
                                 i0: Int,
                                 len0: Int,
                                 naiveOutLen: Int,
                                 wrapQuotes: Boolean): Unit = escapeSingleByteUnicodeRaw(
    unicodeCharBuilder, sb, s, escapeUnicode, i0, len0, wrapQuotes
  )

  def escapeSingleByteUnicodeRaw(unicodeCharBuilder: CharBuilder,
                                 sb: ByteBuilder,
                                 s: CharSequence,
                                 escapeUnicode: Boolean,
                                 i0: Int,
                                 len0: Int,
                                 wrapQuotes: Boolean): Unit = {
    // Take the unicodeCharBuilder's Array[Char] containing potentially unicode characters and write
    // them as UTF-8 bytes in sb's Array[Byte]
    //
    // Somehow aggregating the unicode string in `unicodeCharBuilder` and then
    // looping over the chars here is more than twice as fast as doing the same
    // logic in the main `escapeByte` work loop. It's also a lot faster than
    // calling `.makeString().getBytes(UTF_8)` on `unicodeCharBuilder`, since
    // we don't allocate a temporary String and Array[Byte]
    //
    // Taken from https://stackoverflow.com/a/9670279

    unicodeCharBuilder.reset()
    escapeChar0(i0, len0, unicodeCharBuilder, s, escapeUnicode, wrapQuotes)

    val xs: Array[Char] = unicodeCharBuilder.arr
    val len = unicodeCharBuilder.length
    sb.ensureLength(len * 3) // worst case
    val ys: Array[Byte] = sb.arr
    var i = 0
    var j = sb.length // i for chars; j for bytes
    while (i < len) { // fill ys with bytes
      val c = xs(i)
      if (c < 0x80) {
        ys(j) = c.toByte
        i = i + 1
        j = j + 1
      } else if (c < 0x800) {
        ys(j) = (0xc0 | (c >> 6)).toByte
        ys(j + 1) = (0x80 | (c & 0x3f)).toByte
        i = i + 1
        j = j + 2
      } else if (Character.isHighSurrogate(c)) {
        if (len - i < 2) throw new Exception("overflow")
        val d = xs(i + 1)
        val uc: Int =
          if (Character.isLowSurrogate(d)) Character.toCodePoint(c, d)
          else throw new Exception("malformed")
        ys(j) = (0xf0 | ((uc >> 18))).toByte
        ys(j + 1) = (0x80 | ((uc >> 12) & 0x3f)).toByte
        ys(j + 2) = (0x80 | ((uc >> 6) & 0x3f)).toByte
        ys(j + 3) = (0x80 | (uc & 0x3f)).toByte
        i = i + 2 // 2 chars
        j = j + 4
      } else if (Character.isLowSurrogate(c)) {
        throw new Exception("malformed")
      } else {
        ys(j) = (0xe0 | (c >> 12)).toByte
        ys(j + 1) = (0x80 | ((c >> 6) & 0x3f)).toByte
        ys(j + 2) = (0x80 | (c & 0x3f)).toByte
        i = i + 1
        j = j + 3
      }
    }
    sb.length = j
  }

  def escapeChar(unicodeCharBuilder: upickle.core.CharBuilder,
                 sb: upickle.core.CharBuilder,
                 s: CharSequence,
                 escapeUnicode: Boolean,
                 wrapQuotes: Boolean) = {
    val len = s.length
    if (wrapQuotes) sb.append('"')
    escapeChar0(0, len, sb, s, escapeUnicode, wrapQuotes)
  }

  final def escapeChar0(i0: Int,
                        len: Int,
                        sb: upickle.core.CharBuilder,
                        s: CharSequence,
                        escapeUnicode: Boolean,
                        wrapQuotes: Boolean): upickle.core.CharBuilder = {
    var i = i0

    // Split the loop based on `escapeUnicode`,
    // to avoid repeating the check within the hot loop
    if (escapeUnicode){
      while (i < len) {
        val c = s.charAt(i)
        if (!CharUtils.appendEscapedChar(sb, c, i)) {
          if (c < ' ' || c > 127) { CharUtils.escapeSingleCharUnicodeEscape(sb, i, c) }
          else { i = CharUtils.appendSimpleStringSectionNoUnicode(sb, i, len, s) }
        }
        i += 1
      }
    }else{
      while (i < len) {
        val c = s.charAt(i)
        if (!CharUtils.appendEscapedChar(sb, c, i)) {
          if (c < ' ') { CharUtils.escapeSingleCharUnicodeEscape(sb, i, c) }
          else { i = CharUtils.appendSimpleStringSection(sb, i, len, s) }
        }
        i += 1
      }
    }

    if (wrapQuotes) sb.append('"')
    sb
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
    while(i <= 10){
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
    while (i <= 18){
      if (x > p) return i + d
      p = 10 * p
      i += 1
    }
    19 + d
  }

  @deprecated("Not used, kept for binary compatibility")
  def escapeSingleChar(sb: upickle.core.CharBuilder,
                       naiveOutLen: Int,
                       i: Int,
                       c: Char) = CharUtils.escapeSingleChar(sb, i, c)

  @deprecated("Not used, kept for binary compatibility")
  def escapeSingleByte(sb: upickle.core.ByteBuilder,
                       naiveOutLen: Int,
                       i: Int,
                       c: Char) = ByteUtils.escapeSingleByte(sb, i, c)

  @deprecated("Not used, kept for binary compatibility")
  def escapeSingleCharUnicodeEscape(naiveOutLen: Int, sb: CharBuilder, i: Int, c: Char) =
    CharUtils.escapeSingleCharUnicodeEscape(sb, i, c)

  @deprecated("Not used, kept for binary compatibility")
  def escapeSingleByteUnicodeEscape(sb: ByteBuilder, i: Int, naiveOutLen: Int, c: Char) = {
    ByteUtils.escapeSingleByteUnicodeEscape(sb, i, c)
  }

  @deprecated("Not used, kept for binary compatibility")
  final def escapeChar0(i0: Int,
                        naiveOutLen: Int,
                        len: Int,
                        sb: upickle.core.CharBuilder,
                        s: CharSequence,
                        escapeUnicode: Boolean,
                        wrapQuotes: Boolean): upickle.core.CharBuilder = {
    escapeChar0(i0, len, sb, s, escapeUnicode, wrapQuotes)
  }

}
