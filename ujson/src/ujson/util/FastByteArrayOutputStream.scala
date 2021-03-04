package ujson.util

/**
  * A version of `java.util.ByteArrayOutputStream` which lets you use the internal
  * Array directly for constructing `String`s
  */
class FastByteArrayOutputStream(startSize: Int = 32){
  private[this] var arr: Array[Byte] = new Array(startSize)
  private[this] var length: Int = 0
  def reset() = length = 0
  def write(x: Byte) = {
    if (length == arr.length) arr = java.util.Arrays.copyOf(arr, arr.length * 2)
    arr(length) = x
    length += 1
  }
  def makeString() = new String(arr, 0, length)
}
