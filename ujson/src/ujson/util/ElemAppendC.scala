package ujson.util

abstract class CharAppendC {
  def append(x: Char)
  def appendC(x: Char): Unit = append(x)

}
abstract class ByteAppendC {
  def append(x: Byte)
  def appendC(x: Char): Unit = {
    if (x < 0x80) { // Have at most seven bits
      append(x.toByte)
    } else if (x < 0x800) { // 2 bytes, 11 bits
      append((0xc0 | (x >> 6)).toByte)
      append((0x80 | (x & 0x3f)).toByte)
    } else if (Character.isSurrogate(x)) { // Have a surrogate pair
      append((0xc0 | (x >> 6)).toByte)
      append((0x80 | (x & 0x3f)).toByte)
      //        if (sgp == null) sgp = new Surrogate.Parser
      //        val uc = sgp.parse(x, src)
      //        append((0xf0 | uc >> 18).toByte)
      //        append((0x80 | ((uc >> 12) & 0x3f)).toByte)
      //        append((0x80 | ((uc >> 6) & 0x3f)).toByte)
      //        append((0x80 | (uc & 0x3f)).toByte)
    }
    else { // 3 bytes, 16 bits
      append((0xe0 | x >> 12).toByte)
      append((0x80 | ((x >> 6) & 0x3f)).toByte)
      append((0x80 | (x & 0x3f)).toByte)
    }
  }
}