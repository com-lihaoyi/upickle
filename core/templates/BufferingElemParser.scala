package upickle.core


trait BufferingElemParser{

  private[this] var buffer: Array[Elem] = null

  private[this] var firstIdx = 0
  private[this] var lastIdx = 0
  private[this] var dropped = 0

  def getLastIdx = lastIdx

  def getElemSafe(i: Int): Elem = {
    requestUntil(i)
    buffer(i - firstIdx)
  }
  def getElemUnsafe(i: Int): Elem = {
    buffer(i - firstIdx)
  }

  def sliceString(i: Int, k: Int): String = {
    requestUntil(k)
    println("buffer.toSeq " + buffer.toSeq)
    println("i " + i)
    println("k " + k)
    println("firstIdx " + firstIdx)
    println("buffer.slice(i - firstIdx, k - firstIdx) " + buffer.slice(i - firstIdx, k - firstIdx).toSeq)
    new String(buffer.slice(i - firstIdx, k - firstIdx))
  }

  def sliceArr(i: Int, n: Int): (Array[Elem], Int, Int) = {
    requestUntil(i + n)
    val arr = buffer.slice(i - firstIdx, i + n - firstIdx)
    (arr, 0, arr.length)
  }

  def growBuffer(until: Int) = {
    var newSize = buffer.length

    // Bump growGoalSiz by 50%. This helps ensure the utilization of the buffer
    // ranges from 33% to 66%, rather than from 50% to 100%. We want to avoid being
    // near 100% because we could end up doing large numbers of huge System.arraycopy
    // calls even when processing tiny amounts of data
    val growGoalSize = (until - dropped + 1) * 3 / 2
    while (newSize <= growGoalSize) newSize *= 2

    val arr = if (newSize > buffer.length / 2) new Array[Elem](newSize) else buffer

    System.arraycopy(buffer, dropped - firstIdx, arr, 0, lastIdx - dropped)
    firstIdx = dropped
    buffer = arr
  }
  protected def requestUntil(until: Int): Boolean = {

    if (until < lastIdx) false
    else {
      val untilBufferOffset = until - firstIdx
      if (buffer != null && untilBufferOffset >= buffer.length) growBuffer(until)


      val bufferOffset = lastIdx - firstIdx
      val (newBuffer, newDone, n) = readDataIntoBuffer(buffer, bufferOffset)
      buffer = newBuffer

      lastIdx = lastIdx + n
      newDone
    }
  }
  def readDataIntoBuffer(buffer: Array[Elem], bufferOffset: Int): (Array[Elem], Boolean, Int)

  def dropBufferUntil(i: Int): Unit = {
    dropped = i
  }
  def unsafeCharSeqForRange(start: Int, length: Int) = {
    new WrapElemArrayCharSeq(buffer, start - firstIdx, length)
  }

  def appendElemsToBuilder(elems: ElemBuilder, elemsStart: Int, elemsLength: Int) = {
    elems.appendAll(buffer, elemsStart - firstIdx, elemsLength)
  }
}
