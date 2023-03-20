package upickle.core


/**
  * A fast buffer that can be used to store Elems (Bytes or Chars).
  *
  * Generally faster than the equivalent [[StringBuilder]] or
  * [[java.io.ByteArrayOutputStream]], since:
  *
  * - It is specialized and without the overhead of polymorphism or synchronization.
  * - It allows the user to call `ensureLength` and `appendUnsafe` separately, e.g.
  *   callign `ensureLength` once before `appendUnsafe`-ing multiple Elems
  * - It provides fast methods like [[makeString]] or [[writeOutToIfLongerThan]], that
  *   let you push the data elsewhere with minimal unnecessary copying
  */
class ElemBuilder(startSize: Int = 32) extends upickle.core.ElemAppendC{
  var arr: Array[Elem] = new Array(startSize)
  var length: Int = 0
  private def getArr = arr
  def getLength = length
  def reset(): Unit = length = 0
  def ensureLength(increment: Int): Unit = {
    var multiple = arr.length
    val targetLength = length + increment
    while (multiple < targetLength) multiple = multiple * 2
    if (multiple != arr.length) arr = java.util.Arrays.copyOf(arr, multiple)
  }
  def append(x: Int): Unit = append(x.toElem)
  def append(x: Elem): Unit = {
    if (length == arr.length) arr = java.util.Arrays.copyOf(arr, arr.length * 2)
    arr(length) = x
    length += 1
  }
  def appendAll(elems: Array[Elem], elemsLength: Int): Unit = appendAll(elems, 0, elemsLength)

  def appendAll(elems: Array[Elem], elemsStart: Int, elemsLength: Int): Unit = {
    ensureLength(elemsLength)
    System.arraycopy(elems, elemsStart, arr, length, elemsLength)
    length += elemsLength
  }
  def appendAllUnsafe(other: ElemBuilder): Unit = {
    val elemsLength = other.getLength
    System.arraycopy(other.getArr, 0, arr, length, elemsLength)
    length += elemsLength
  }

  def appendUnsafeC(x: Char): Unit = appendUnsafe(x.toElem)
  def appendUnsafe(x: Elem): Unit = {
    arr(length) = x
    length += 1
  }

  def makeString(): String = ElemOps.newString(arr, 0, length)
  def writeOutToIfLongerThan(writer: upickle.core.ElemOps.Output, threshold: Int): Unit = {
    if (length > threshold) {
      writer.write(arr, 0, length)
      length = 0
    }
  }

}
