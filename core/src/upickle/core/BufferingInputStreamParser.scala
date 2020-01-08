package upickle.core

/**
  * Defines common functionality to any parser that works on a `java.io.InputStream`
  *
  * Allows you to look up individual bytes by index, take slices of byte ranges or
  * strings, and drop old portions of buffered data once you are certain you no
  * longer need them.
  *
  * The `buffer` size is managed by allowing it to grow in size until it exceeds its
  * capacity. When that happens, one of two things happen:
  *
  * - If the buffer has enough space, we left-shift the data in the
  *   buffer to over-write the portion that has already been dropped.
  *
  * - If the buffer does not have enough space, we allocate a new buffer big
  *   enough to hold the new data we need to store (size a power of two multiple of
  *   the old size) and copy the data over, again shifted left
  * .
  */
trait BufferingInputStreamParser{
  def maxStartBufferSize: Int
  def minStartBufferSize: Int
  def data: java.io.InputStream
  private[this] var buffer = new Array[Byte]({
    val available = data.available()
    // + 1 to make sure we always have just a bit more space than is necessary to
    // store the entire stream contents. This is necessary to avoid trying to
    // check for EOF when the entire buffer is full, which fails on Scala.js due to
    //
    // - https://github.com/scala-js/scala-js/issues/3913
    if (available + 1 < minStartBufferSize) minStartBufferSize
    else if (available + 1 > maxStartBufferSize) maxStartBufferSize
    else available + 1
  })

  private[this] var firstIdx = 0
  private[this] var lastIdx = 0
  private[this] var dropped = 0

  def getLastIdx = lastIdx

  def byte(i: Int): Byte = {
    requestUntil(i)
    upickle.core.Platform.byteAt(buffer, i - firstIdx)
  }

  def sliceString(i: Int, k: Int): String = {
    requestUntil(k)
    new String(buffer.slice(i - firstIdx, k - firstIdx))
  }

  def sliceBytes(i: Int, n: Int): (Array[Byte], Int, Int) = {
    requestUntil(i + n)
    val arr = buffer.slice(i - firstIdx, i + n - firstIdx)
    (arr, 0, arr.length)
  }

  protected def requestUntil(until: Int): Boolean = {
    // + 1 to make sure we always have just a bit more space than is necessary to
    // store the entire stream contents. This is necessary to avoid trying to
    // check for EOF when the entire buffer is full, which fails on Scala.js due to
    //
    // - https://github.com/scala-js/scala-js/issues/3913
    val untilBufferOffset = until - firstIdx + 1
    if (untilBufferOffset >= buffer.size){
      var newSize = buffer.length

      val growGoalSize = (until - dropped + 1)
      while (newSize <= growGoalSize) newSize *= 2

      val arr = if (newSize > buffer.length) new Array[Byte](newSize) else buffer

      System.arraycopy(buffer, dropped - firstIdx, arr, 0, lastIdx - dropped)
      firstIdx = dropped
      buffer = arr
    }

    var done = false
    while (lastIdx <= until && !done) done = readDataIntoBuffer()
    done
  }
  def readDataIntoBuffer(): Boolean = {
    var done = false
    val bufferOffset = lastIdx - firstIdx
    data.read(buffer, bufferOffset, buffer.length - bufferOffset)  match{
      case -1 => done = true
      case n => lastIdx += n
    }
    done
  }

  def dropBufferUntil(i: Int): Unit = {
    dropped = i
  }
}
object BufferingInputStreamParser{
  val defaultMaxBufferStartSize: Int = 64 * 1024
  val defaultMinBufferStartSize: Int = 64
}
