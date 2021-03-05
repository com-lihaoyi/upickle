package ujson.util

import java.nio.charset.StandardCharsets

import scala.annotation.switch

object RenderUtils{
  private[ujson] final val hexChars: Array[Int] = {
    val arr = new Array[Int](128)
    var i = 0
    while (i < 10) { arr(i + '0') = i; i += 1 }
    i = 0
    while (i < 16) { arr(i + 'a') = 10 + i; arr(i + 'A') = 10 + i; i += 1 }
    arr
  }

  private def toHex(nibble: Int): Char = (nibble + (if (nibble >= 10) 87 else 48)).toChar

  final def escapeByte(sb: ujson.util.ByteBuilder,
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
              val cb = new ujson.util.CharBuilder()
              escapeChar0(i, cb, s, unicode)

              val bytes = cb.makeString().getBytes(StandardCharsets.UTF_8)
              sb.appendAll(bytes, bytes.length)
              return
            }
          }
      }
      i += 1
    }
    sb.appendUnsafe('"')
  }

  final def escapeChar(sb: ujson.util.CharBuilder,
                       s: CharSequence,
                       unicode: Boolean) = {
    val len = s.length
    val naiveOutLen = len + 2 // +2 for the start and end quotes
    sb.ensureLength(naiveOutLen)
    sb.appendUnsafe('"')
    escapeChar0(0, sb, s, unicode)
  }
  final def escapeChar0(i0: Int,
                        sb: ujson.util.CharBuilder,
                        s: CharSequence,
                        unicode: Boolean) = {
    var i = i0
    val len = s.length
    val naiveOutLen = len + 2 // +2 for the start and end quotes
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
