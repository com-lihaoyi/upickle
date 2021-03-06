package upickle.core


trait BufferingElemParser{
  def maxStartBufferSize: Int
  def minStartBufferSize: Int
  def defaultStartBufferSize: Int
  private[this] var buffer: Array[Elem] = null

  def instantiateBuffer() = {
    new Array[Elem]({
      val available = bumpBufferSize(defaultStartBufferSize)
      if (available < minStartBufferSize) minStartBufferSize
      else if (available > maxStartBufferSize) maxStartBufferSize
      else available
    })
  }
  private[this] var firstIdx = 0
  private[this] var lastIdx = 0
  private[this] var dropped = 0

  // + 1 to make sure we always have just a bit more space than is necessary to
  // store the entire stream contents. This is necessary to avoid trying to
  // check for EOF when the entire buffer is full, which fails on Scala.js due to
  //
  // - https://github.com/scala-js/scala-js/issues/3913
  private[this] def bumpBufferSize(n: Int) = n + 1

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
    val untilBufferOffset = bumpBufferSize(until - firstIdx)
    if (buffer != null && untilBufferOffset >= buffer.length) growBuffer(until)

    var done = false
    while (lastIdx <= until && !done) {
      val (newBuffer, newDone, newLastIdx) = readDataIntoBuffer(buffer, firstIdx, lastIdx)
      buffer = newBuffer
      done = newDone
      lastIdx = newLastIdx
    }
    done
  }
  def readDataIntoBuffer(buffer: Array[Elem], firstIdx: Int, lastIdx: Int): (Array[Elem], Boolean, Int)

  def dropBufferUntil(i: Int): Unit = {
    dropped = i
  }
}
