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

  final def escapeByte(unicodeCharBuilder: upickle.core.CharBuilder,
                       sb: upickle.core.ByteBuilder,
                       s: CharSequence,
                       unicode: Boolean): Unit = {

    var i = 0
    val len = s.length
    val naiveOutLen = len + 2 // +2 for the start and end quotes
    sb.ensureLength(naiveOutLen)
    sb.appendUnsafe('"')
    while (i < len) {
      (s.charAt(i): @switch) match {
        case '"' => sb.ensureLength(naiveOutLen - i + 1); sb.appendUnsafeC('\\'); sb.appendUnsafeC('\"')
        case '\\' => sb.ensureLength(naiveOutLen - i + 1); sb.appendUnsafeC('\\'); sb.appendUnsafeC('\\')
        case '\b' => sb.ensureLength(naiveOutLen - i + 1); sb.appendUnsafeC('\\'); sb.appendUnsafeC('b')
        case '\f' => sb.ensureLength(naiveOutLen - i + 1); sb.appendUnsafeC('\\'); sb.appendUnsafeC('f')
        case '\n' => sb.ensureLength(naiveOutLen - i + 1); sb.appendUnsafeC('\\'); sb.appendUnsafeC('n')
        case '\r' => sb.ensureLength(naiveOutLen - i + 1); sb.appendUnsafeC('\\'); sb.appendUnsafeC('r')
        case '\t' => sb.ensureLength(naiveOutLen - i + 1); sb.appendUnsafeC('\\'); sb.appendUnsafeC('t')
        case c =>
          if (c < ' ' || (c > '~' && unicode)) {
            sb.ensureLength(naiveOutLen - i + 5);
            sb.appendUnsafeC('\\')
            sb.appendUnsafeC('u')
            sb.appendUnsafeC(toHex((c >> 12) & 15))
            sb.appendUnsafeC(toHex((c >> 8) & 15))
            sb.appendUnsafeC(toHex((c >> 4) & 15))
            sb.appendUnsafeC(toHex(c & 15))
          } else {
            if (c <= 127) sb.append(c)
            else{
              unicodeCharBuilder.reset()
              escapeChar0(i, naiveOutLen, len, unicodeCharBuilder, s, unicode)

              val bytes = unicodeCharBuilder.makeString().getBytes(StandardCharsets.UTF_8)
              sb.appendAll(bytes, bytes.length)
              return
            }
          }
      }
      i += 1
    }
    sb.appendUnsafe('"')
  }
  final def encodeCharSequenceToBytes(unicodeCharBuilder: upickle.core.CharBuilder,
                                      sb: upickle.core.ByteBuilder,
                                      s: CharSequence): Unit = {

    var i = 0
    val naiveOutLen = s.length
    sb.ensureLength(naiveOutLen)
    while (i < naiveOutLen) {
      (s.charAt(i): @switch) match {
        case c =>
          if (c <= 127) sb.append(c)
          else{
            unicodeCharBuilder.reset()
            while(i < naiveOutLen){
              unicodeCharBuilder.append(s.charAt(i))
              i += 1
            }
            val bytes = unicodeCharBuilder.makeString().getBytes(StandardCharsets.UTF_8)
            sb.appendAll(bytes, bytes.length)
            return
          }
      }
      i += 1
    }
  }

  final def escapeChar(unicodeCharBuilder: upickle.core.CharBuilder,
                       sb: upickle.core.CharBuilder,
                       s: CharSequence,
                       unicode: Boolean) = {
    val len = s.length
    val naiveOutLen = len + 2 // +2 for the start and end quotes
    sb.ensureLength(naiveOutLen)
    sb.appendUnsafe('"')
    escapeChar0(0, naiveOutLen, len, sb, s, unicode)
  }
  final def escapeChar0(i0: Int,
                        naiveOutLen: Int,
                        len: Int,
                        sb: upickle.core.CharBuilder,
                        s: CharSequence,
                        unicode: Boolean) = {
    var i = i0
    sb.ensureLength(naiveOutLen)
    while (i < len) {
      (s.charAt(i): @switch) match {
        case '"' => sb.ensureLength(naiveOutLen - i + 1); sb.appendUnsafeC('\\'); sb.appendUnsafeC('\"')
        case '\\' => sb.ensureLength(naiveOutLen - i + 1); sb.appendUnsafeC('\\'); sb.appendUnsafeC('\\')
        case '\b' => sb.ensureLength(naiveOutLen - i + 1); sb.appendUnsafeC('\\'); sb.appendUnsafeC('b')
        case '\f' => sb.ensureLength(naiveOutLen - i + 1); sb.appendUnsafeC('\\'); sb.appendUnsafeC('f')
        case '\n' => sb.ensureLength(naiveOutLen - i + 1); sb.appendUnsafeC('\\'); sb.appendUnsafeC('n')
        case '\r' => sb.ensureLength(naiveOutLen - i + 1); sb.appendUnsafeC('\\'); sb.appendUnsafeC('r')
        case '\t' => sb.ensureLength(naiveOutLen - i + 1); sb.appendUnsafeC('\\'); sb.appendUnsafeC('t')
        case c =>
          if (c < ' ' || (c > '~' && unicode)) {
            sb.ensureLength(naiveOutLen - i + 5);
            sb.appendUnsafeC('\\')
            sb.appendUnsafeC('u')
            sb.appendUnsafeC(toHex((c >> 12) & 15))
            sb.appendUnsafeC(toHex((c >> 8) & 15))
            sb.appendUnsafeC(toHex((c >> 4) & 15))
            sb.appendUnsafeC(toHex(c & 15))
          } else {
            sb.append(c)
          }
      }
      i += 1
    }
    sb.appendUnsafe('"')
    sb
  }

}
