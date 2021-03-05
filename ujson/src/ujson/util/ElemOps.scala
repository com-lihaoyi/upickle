package ujson.util

object CharOps {
  def toInt(c: Char) = c.toInt
  def toUnsignedInt(c: Char) = c.toInt
  def lessThan(c1: Char, c2: Char) = c1 < c2
  def within(c1: Char, c2: Char, c3: Char) = c1 <= c2 && c2 <= c3
  type Output = java.io.Writer

}
object ByteOps {
  def toInt(c: Byte) = c.toInt
  def toUnsignedInt(c: Byte) = c & 0xff
  def lessThan(c1: Byte, c2: Char) = c1 < c2
  def within(c1: Char, c2: Byte, c3: Char) = c1 <= c2 && c2 <= c3
  type Output = java.io.OutputStream
}