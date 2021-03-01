package ujson.borer


import ujson.borer.internal.ByteArrayAccess

import scala.annotation.tailrec

object Utf8 {

  def encode(value: Array[Char]): Array[Byte] = {

    @tailrec def rec(si: Int, di: Int, result: Array[Byte]): Array[Byte] =
      if (si < value.length) {
        val c = value(si)
        if (c > 0x7F) {
          var codePoint = c.toInt
          var nsi       = si
          var ndi       = di
          val nresult   = if (di < result.length - 3) result else java.util.Arrays.copyOf(result, (di + 2) << 1)
          if (codePoint > 0x7FF) {
            if (0xD800 <= codePoint && codePoint < 0xE000) { // UTF-16 high surrogate (i.e. first of pair)
              if (codePoint >= 0xDC00) sys.error(s"Invalid surrogate pair at index $si")
              nsi += 1
              if (nsi >= value.length) sys.error("Truncated UTF-16 surrogate pair at end of char array")
              codePoint = Character.toCodePoint(c, value(nsi))
              nresult(ndi) = (0xF0 | (codePoint >> 18)).toByte
              ndi += 1
              nresult(ndi) = (0x80 | ((codePoint >> 12) & 0x3F)).toByte
            } else nresult(ndi) = (0xE0 | (codePoint >> 12)).toByte // 3-byte UTF-8 codepoint
            ndi += 1
            nresult(ndi) = (0x80 | ((codePoint >> 6) & 0x3F)).toByte
          } else nresult(ndi) = (0xC0 | (codePoint >> 6)).toByte // 2-byte UTF-8 codepoint
          nresult(ndi + 1) = (0x80 | (codePoint & 0x3F)).toByte
          rec(nsi + 1, ndi + 2, nresult)
        } else {
          val nresult = if (di < result.length) result else java.util.Arrays.copyOf(result, di << 1)
          nresult(di) = c.toByte
          rec(si + 1, di + 1, nresult)
        }
      } else if (di < result.length) {
        java.util.Arrays.copyOfRange(result, 0, di)
      } else result

    if (value.length > 0) {
      rec(0, 0, new Array[Byte](value.length)) // optimistic: maybe we don't have any non-ASCII chars at all
    } else Array.emptyByteArray
  }

  def decode(bytes: Array[Byte]): Array[Char] = {
    val result = new Array[Char](bytes.length) // upper bound: we'll never need more than bytes.length chars
    val baa    = ByteArrayAccess.instance
    val sl8    = bytes.length - 8

    def fail(si: Int) = sys.error(s"Illegal UTF-8 character encoding at byte index $si")

    @tailrec def decode8bit(b1: Int, si: Int, di: Int): Long = {
      val byteCount = Integer.numberOfLeadingZeros(~b1) - 25
      var ndi       = di
      val quad =
        bytes.length - si match {
          case 0 => 0
          case 1 => bytes(si) << 24
          case 2 => baa.doubleByteBigEndian(bytes, si) << 16
          case 3 => baa.doubleByteBigEndian(bytes, si) << 16 | (bytes(si + 2) & 0xFF) << 8
          case _ => baa.quadByteBigEndian(bytes, si)
        }
      val b2 = quad >> 24
      val cp = (byteCount | 0x80) ^ (b2 & 0xC0) match {
        case 1 =>
          if ((b1 & 0x1E) == 0) fail(si)
          (b1 << 6) ^ b2 ^ 0xF80
        case 2 =>
          val b3 = quad << 8 >> 24
          val c  = (b1 << 12) ^ (b2 << 6) ^ b3 ^ 0xFFFE1F80
          if ((b1 == 0xE0 && (b2 & 0xE0) == 0x80) || (b3 & 0xC0) != 0x80 || ((c >> 11) == 0x1B)) fail(si)
          c
        case 3 =>
          val b3 = quad << 8 >> 24
          val b4 = quad << 16 >> 24
          val c  = (b1 << 18) ^ (b2 << 12) ^ (b3 << 6) ^ b4 ^ 0x381F80
          if ((b3 & 0xC0) != 0x80 || (b4 & 0xC0) != 0x80 || c < 0x010000 || c > 0x10FFFF) fail(si)
          result(ndi) = (0xD7C0 + (c >> 10)).toChar // high surrogate
          ndi += 1
          0xDC00 + (c & 0x3FF) // low surrogate
        case _ => fail(si)
      }
      result(ndi) = cp.toChar
      val nsi = si + byteCount
      ndi += 1

      // if the next byte is also an 8-bit character (which is not that unlikely) we decode that as well right away
      val nextByte = quad << (byteCount << 3) >> 24
      if (nextByte >= 0) { // no 8-bit character
        (nsi.toLong << 32) | ndi.toLong
      } else decode8bit(nextByte, nsi + 1, ndi) // nextByte is an 8-bit character, so recurse
    }

    @tailrec def decode1(si: Int, di: Int): Array[Char] =
      if (si < bytes.length) {
        val b = bytes(si).toInt
        if (b < 0) { // 8-bit
          val sidi = decode8bit(b, si + 1, di)
          decode1((sidi >>> 32).toInt, sidi.toInt)
        } else { // 7-bit
          result(di) = b.toChar
          decode1(si + 1, di + 1)
        }
      } else if (di < result.length) {
        java.util.Arrays.copyOfRange(result, 0, di)
      } else result

    @tailrec def decode8(si: Int, di: Int): Array[Char] =
      if (si <= sl8) {
        // fetch 8 bytes (chars) at the same time with the first becoming the (left-most) MSB of the `octa` long
        val octa      = baa.octaByteBigEndian(bytes, si)
        val nlz       = java.lang.Long.numberOfLeadingZeros(octa & 0x8080808080808080L)
        val charCount = nlz >> 3 // the number of 7-bit chars before a (potential) 8-bit char [0..8]

        // in order to decrease instruction dependencies we always speculatively write all 8 chars to the char buffer,
        // independently of how many are actually "good" chars, this keeps CPU pipelines maximally busy
        result(di + 0) = (octa << 0 >>> 56).toChar
        result(di + 1) = (octa << 8 >>> 56).toChar
        result(di + 2) = (octa << 16 >>> 56).toChar
        result(di + 3) = (octa << 24 >>> 56).toChar
        result(di + 4) = (octa << 32 >>> 56).toChar
        result(di + 5) = (octa << 40 >>> 56).toChar
        result(di + 6) = (octa << 48 >>> 56).toChar
        result(di + 7) = (octa & 0xFFL).toChar

        if (nlz < 64) { // do we have an 8-bit char anywhere?
          val b1   = (octa << nlz >> 56).toInt // the first 8-bit char after `charCount` 7-bit chars
          val sidi = decode8bit(b1, si + charCount + 1, di + charCount)
          decode8((sidi >>> 32).toInt, sidi.toInt)
        } else decode8(si + 8, di + 8) // we have written 8 normal chars, so recurse
      } else decode1(si, di)

    decode8(0, 0)
  }

}