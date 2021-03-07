package ujson.util

import java.nio.charset.StandardCharsets

import scala.annotation.switch

object RenderUtils{
  import upickle.core.Util.hexChar
  final def escapeByte(unicodeCharBuilder: ujson.util.CharBuilder,
                       sb: ujson.util.ByteBuilder,
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
            sb.appendUnsafeC(hexChar((c >> 12) & 15).toChar)
            sb.appendUnsafeC(hexChar((c >> 8) & 15).toChar)
            sb.appendUnsafeC(hexChar((c >> 4) & 15).toChar)
            sb.appendUnsafeC(hexChar(c & 15).toChar)
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

  final def escapeChar(unicodeCharBuilder: ujson.util.CharBuilder,
                       sb: ujson.util.CharBuilder,
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
                        sb: ujson.util.CharBuilder,
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
            sb.appendUnsafeC(hexChar((c >> 12) & 15).toChar)
            sb.appendUnsafeC(hexChar((c >> 8) & 15).toChar)
            sb.appendUnsafeC(hexChar((c >> 4) & 15).toChar)
            sb.appendUnsafeC(hexChar(c & 15).toChar)
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
