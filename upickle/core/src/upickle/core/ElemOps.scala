package upickle.core
import java.nio.charset.StandardCharsets

object CharOps {
  def toInt(c: Char) = c.toInt
  def toUnsignedInt(c: Char) = c.toInt
  def lessThan(c1: Char, c2: Char) = c1 < c2
  def within(c1: Char, c2: Char, c3: Char) = c1 <= c2 && c2 <= c3
  type Output = java.io.Writer
  def newString(arr: Array[Char], i: Int, length: Int) = new String(arr, i, length)

}
object ByteOps {
  def toInt(c: Byte) = c.toInt
  def toUnsignedInt(c: Byte) = c & 0xff
  def lessThan(c1: Byte, c2: Char) = c1 < c2
  def within(c1: Char, c2: Byte, c3: Char) = c1 <= c2 && c2 <= c3
  type Output = java.io.OutputStream
  def newString(arr: Array[Byte], i: Int, length: Int) = new String(arr, i, length, StandardCharsets.UTF_8)
}