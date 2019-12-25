package upickle.core

trait BufferingInputStreamParser{
  def bufferSize: Int
  def data: java.io.InputStream
  private[this] var buffer = new Array[Byte](bufferSize)

  private[this] var firstIdx = 0
  private[this] var lastIdx = 0
  private[this] var dropped = 0


  def getLastIdx = lastIdx

  def byte(i: Int): Byte = {
    requestUntil(i)
    buffer.apply(i - firstIdx)
  }

  def sliceString(i: Int, n: Int): String = {
    requestUntil(i + n)
    new String(buffer.slice(i - firstIdx, i + n - firstIdx))
  }

  def sliceBytes(i: Int, n: Int): (Array[Byte], Int, Int) = {
    requestUntil(i + n)
    val arr = buffer.slice(i - firstIdx, i + n - firstIdx)
    (arr, 0, arr.length)
  }

  protected def requestUntil(until: Int): Boolean = {
    val requiredSize = until - firstIdx
    if (requiredSize >= buffer.size){
      var newSize = buffer.length
      while (newSize <= requiredSize) newSize *= 2

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
    data.read(buffer, bufferOffset, buffer.length - bufferOffset) match{
      case -1 => done = true
      case n => lastIdx += n
    }
    done
  }

  def dropBufferUntil(i: Int): Unit = {
    dropped = i
  }
}