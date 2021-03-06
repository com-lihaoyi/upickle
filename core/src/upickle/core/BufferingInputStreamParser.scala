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
trait BufferingInputStreamParser extends BufferingByteParser{
  def data: java.io.InputStream
  def readDataIntoBuffer(buffer: Array[Byte], firstIdx: Int, lastIdx: Int) = {
    val newBuffer = if (buffer == null) instantiateBuffer() else buffer
    val bufferOffset = lastIdx - firstIdx
    data.read(newBuffer, bufferOffset, newBuffer.length - bufferOffset)  match{
      case -1 => (newBuffer, true, lastIdx)
      case n => (newBuffer, false, lastIdx + n)
    }
  }
  def sliceBytes(i: Int, n: Int) = sliceArr(i, n)

  def defaultStartBufferSize = data.available()
}
object BufferingInputStreamParser{
  val defaultMaxBufferStartSize: Int = 64 * 1024
  val defaultMinBufferStartSize: Int = 64
}