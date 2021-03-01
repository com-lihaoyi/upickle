package ujson.borer.internal


/**
  * A mutable RingBuffer that can grow in size.
  * Contrary to many other ring buffer implementations this one does not automatically overwrite the oldest
  * elements, rather, if full, the buffer tries to grow and rejects further writes if max capacity is reached.
  *
  * @param initialCapacity the initial buffer size
  * @param maxCapacity the maximum number of elements the buffer can hold.
  */
final private[borer] class ResizableByteRingBuffer(initialCapacity: Int, val maxCapacity: Int) {
  // automatically implies maxCapacity <= 0x40000000
  if (!Util.isPowerOf2(maxCapacity) || maxCapacity <= 0 || !Util.isPowerOf2(
    initialCapacity) || initialCapacity <= 0 || maxCapacity < initialCapacity)
    throw new IllegalArgumentException

  private[this] var array = new Array[Byte](initialCapacity)
  private[this] var mask  = array.length - 1 // bit mask for converting a cursor into an array index

  /*
   * two counters counting the number of elements ever written and read; wrap-around is
   * handled by always looking at differences or masked values
   */
  private[this] var writeIx: Int = _
  private[this] var readIx: Int  = _

  @inline def clear(): Unit = {
    writeIx = 0
    readIx = 0
  }

  /**
    * The number of elements currently in the buffer.
    */
  @inline def count: Int = writeIx - readIx

  /**
    * True if no elements are currently in the buffer.
    */
  @inline def isEmpty: Boolean = writeIx == readIx

  /**
    * True if at least one elements is currently in the buffer.
    */
  @inline def nonEmpty: Boolean = writeIx != readIx

  /**
    * The number of elements the buffer can hold without having to be resized.
    */
  @inline def currentCapacity: Int = array.length

  def append1(value: Byte): Boolean =
    if (count < currentCapacity) {
      val ix = writeIx
      writeIx = ix + 1
      write1(value, ix)
    } else grow() && append1(value)

  def prepend1(value: Byte): Boolean =
    if (count < currentCapacity) {
      val ix = readIx - 1
      readIx = ix
      write1(value, ix)
    } else grow() && prepend1(value)

  def append2(a: Byte, b: Byte): Boolean =
    if (count <= currentCapacity - 2) {
      val ix = writeIx
      writeIx = ix + 2
      write2(a, b, ix)
    } else grow() && append2(a, b)

  def prepend2(a: Byte, b: Byte): Boolean =
    if (count <= currentCapacity - 2) {
      val ix = readIx - 2
      readIx = ix
      write2(a, b, ix)
    } else grow() && prepend2(a, b)

  def append4(value: Int): Boolean =
    if (count <= currentCapacity - 4) {
      val ix = writeIx
      writeIx = ix + 4
      write4(value, ix)
    } else grow() && append4(value)

  def prepend4(value: Int): Boolean =
    if (count <= currentCapacity - 4) {
      val ix = readIx - 4
      readIx = ix
      write4(value, ix)
    } else grow() && prepend4(value)

  def append5(byte: Byte, int: Int): Boolean =
    if (count <= currentCapacity - 5) {
      val ix = writeIx
      writeIx = ix + 5
      write5(byte, int, ix)
    } else grow() && append5(byte, int)

  def prepend5(byte: Byte, int: Int): Boolean =
    if (count <= currentCapacity - 5) {
      val ix = readIx - 5
      readIx = ix
      write5(byte, int, ix)
    } else grow() && prepend5(byte, int)

  def append8(value: Long): Boolean =
    if (count <= currentCapacity - 8) {
      val ix = writeIx
      writeIx = ix + 8
      write8(value, ix)
    } else grow() && append8(value)

  def prepend8(value: Long): Boolean =
    if (count <= currentCapacity - 8) {
      val ix = readIx - 8
      readIx = ix
      write8(value, ix)
    } else grow() && prepend8(value)

  def append9(byte: Byte, long: Long): Boolean =
    if (count < currentCapacity - 8) {
      val ix = writeIx
      writeIx = ix + 9
      write9(byte, long, ix)
    } else grow() && append9(byte, long)

  def prepend9(byte: Byte, long: Long): Boolean =
    if (count < currentCapacity - 8) {
      val ix = readIx - 9
      readIx = ix
      write9(byte, long, ix)
    } else grow() && prepend9(byte, long)

  @inline private def write1(value: Byte, ix: Int): Boolean = {
    array(ix & mask) = value
    true
  }

  @inline private def write2(a: Byte, b: Byte, ix: Int): Boolean = {
    array(ix & mask) = a
    array((ix + 1) & mask) = b
    true
  }

  private def write4(value: Int, ix: Int): Boolean = {
    val masked = ix & mask
    if (masked <= array.length - 4) {
      ByteArrayAccess.instance.setQuadByteBigEndian(array, masked, value)
    } else {
      array(masked) = (value >> 24).toByte
      array((ix + 1) & mask) = (value >> 16).toByte
      array((ix + 2) & mask) = (value >> 8).toByte
      array((ix + 3) & mask) = (value >> 0).toByte
    }
    true
  }

  private def write5(byte: Byte, int: Int, ix: Int): Boolean = {
    array(ix & mask) = byte
    val masked = (ix + 1) & mask
    if (masked <= array.length - 4) {
      ByteArrayAccess.instance.setQuadByteBigEndian(array, masked, int)
    } else {
      array(masked) = (int >> 24).toByte
      array((ix + 2) & mask) = (int >> 16).toByte
      array((ix + 3) & mask) = (int >> 8).toByte
      array((ix + 4) & mask) = (int >> 0).toByte
    }
    true
  }

  private def write8(value: Long, ix: Int): Boolean = {
    val masked = ix & mask
    if (masked <= array.length - 8) {
      ByteArrayAccess.instance.setOctaByteBigEndian(array, masked, value)
    } else {
      array(masked) = (value >> 56).toByte
      array((ix + 1) & mask) = (value >> 48).toByte
      array((ix + 2) & mask) = (value >> 40).toByte
      array((ix + 3) & mask) = (value >> 32).toByte
      array((ix + 4) & mask) = (value >> 24).toByte
      array((ix + 5) & mask) = (value >> 16).toByte
      array((ix + 6) & mask) = (value >> 8).toByte
      array((ix + 7) & mask) = (value >> 0).toByte
    }
    true
  }

  private def write9(byte: Byte, long: Long, ix: Int): Boolean = {
    array(ix & mask) = byte
    val masked = (ix + 1) & mask
    if (masked <= array.length - 8) {
      ByteArrayAccess.instance.setOctaByteBigEndian(array, masked, long)
    } else {
      array(masked) = (long >> 56).toByte
      array((ix + 2) & mask) = (long >> 48).toByte
      array((ix + 3) & mask) = (long >> 40).toByte
      array((ix + 4) & mask) = (long >> 32).toByte
      array((ix + 5) & mask) = (long >> 24).toByte
      array((ix + 6) & mask) = (long >> 16).toByte
      array((ix + 7) & mask) = (long >> 8).toByte
      array((ix + 8) & mask) = (long >> 0).toByte
    }
    true
  }

  /**
    * Reads the next value from the buffer.
    * Throws a NoSuchElementException if the buffer is empty.
    */
  def readByte(): Byte =
    if (nonEmpty) unsafeReadByte()
    else throw new NoSuchElementException

  /**
    * Reads the next value from the buffer without any buffer underrun protection.
    */
  def unsafeReadByte(): Byte = {
    val r = readIx
    readIx = r + 1
    array(r & mask)
  }

  def unsafeReadQuadByte(): Int = {
    val r = readIx
    readIx = r + 4
    val ix = r & mask
    if (ix <= array.length - 4) {
      ByteArrayAccess.instance.quadByteBigEndian(array, ix)
    } else
      (
        array(ix) << 24
          | (array((r + 1) & mask) & 0xFF) << 16
          | (array((r + 2) & mask) & 0xFF) << 8
          | (array((r + 3) & mask) & 0xFF) << 0
        )
  }

  def unsafeReadOctaByte(): Long = {
    val r = readIx
    readIx = r + 8
    val ix = r & mask
    if (ix <= array.length - 8) {
      ByteArrayAccess.instance.octaByteBigEndian(array, ix)
    } else
      (
        array(ix).toLong << 56
          | (array((r + 1) & mask) & 0xFFL) << 48
          | (array((r + 2) & mask) & 0xFFL) << 40
          | (array((r + 3) & mask) & 0xFFL) << 32
          | (array((r + 4) & mask) & 0xFFL) << 24
          | (array((r + 5) & mask) & 0xFFL) << 16
          | (array((r + 6) & mask) & 0xFFL) << 8
          | (array((r + 7) & mask) & 0xFFL) << 0
        )
  }

  def peekLastOctaByte(): Long =
    if (count >= 8) {
      val w  = writeIx - 8
      val ix = w & mask
      if (ix <= array.length - 8) {
        ByteArrayAccess.instance.octaByteBigEndian(array, ix)
      } else
        (
          array(ix).toLong << 56
            | (array((w + 1) & mask) & 0xFFL) << 48
            | (array((w + 2) & mask) & 0xFFL) << 40
            | (array((w + 3) & mask) & 0xFFL) << 32
            | (array((w + 4) & mask) & 0xFFL) << 24
            | (array((w + 5) & mask) & 0xFFL) << 16
            | (array((w + 6) & mask) & 0xFFL) << 8
            | (array((w + 7) & mask) & 0xFFL) << 0
          )
    } else throw new NoSuchElementException

  def patchLastOctaByte(value: Long): Unit =
    if (count >= 8) write8(value, writeIx - 8)
    else throw new NoSuchElementException

  def dropNext(n: Int): Unit =
    if (n <= count) readIx += n
    else throw new NoSuchElementException

  def dropLast(n: Int): Unit =
    if (n <= count) writeIx -= n
    else throw new NoSuchElementException

  private def grow(): Boolean =
    (array.length < maxCapacity) && {
      val r        = readIx & mask
      val newArray = new Array[Byte](array.length << 1)
      System.arraycopy(array, r, newArray, 0, array.length - r)
      System.arraycopy(array, 0, newArray, array.length - r, r)
      array = newArray
      mask = newArray.length - 1
      writeIx = count
      readIx = 0
      true
    }

  override def toString: String =
    s"ResizableByteRingBuffer(len=${array.length}, size=$count, writeIx=$writeIx, readIx=$readIx)"
}