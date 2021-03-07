package ujson.util

abstract class CharAppendC {
  def append(x: Char)
  def appendC(x: Char): Unit = append(x)

}
abstract class ByteAppendC {
  def append(x: Byte)
  private[this] var surrogate = -1
  def appendC(x: Char): Unit = {

    if (x < 0x80) { // Have at most seven bits
      if (surrogate != -1) {
        throw new Exception("Unexpected character following high surrogate " + x.toInt)
      }
      append(x.toByte)
    } else if (x < 0x800) { // 2 bytes, 11 bits
      if (surrogate != -1) {
        throw new Exception("Unexpected character following high surrogate " + x.toInt)
      }
      append((0xc0 | (x >> 6)).toByte)
      append((0x80 | (x & 0x3f)).toByte)
    } else if (Character.isHighSurrogate(x)) { // Have a surrogate pair
      if(surrogate != -1) throw new Exception("Duplicate high surrogate " + x.toInt)
      surrogate = x
    } else if (Character.isLowSurrogate(x)) { // Have a surrogate pair)
      if(surrogate == -1) throw new Exception("Un-paired low surrogate " + x.toInt)
      val uc = convertSurrogate(surrogate, x)
      surrogate = -1
      append((0xf0 | uc >> 18).toByte)
      append((0x80 | ((uc >> 12) & 0x3f)).toByte)
      append((0x80 | ((uc >> 6) & 0x3f)).toByte)
      append((0x80 | (uc & 0x3f)).toByte)
    } else { // 3 bytes, 16 bits
      surrogate = -1
      append((0xe0 | x >> 12).toByte)
      append((0x80 | ((x >> 6) & 0x3f)).toByte)
      append((0x80 | (x & 0x3f)).toByte)
    }
  }
  def convertSurrogate(firstPart: Int, secondPart: Int) = {
    0x10000 + ((firstPart - Character.MIN_HIGH_SURROGATE) << 10) +
    (secondPart - Character.MIN_LOW_SURROGATE);
  }
}