package ujson.borer.internal


/**
  * A mutable RingBuffer that can grow in size.
  * Contrary to many other ring buffer implementations this one does not automatically overwrite the oldest
  * elements, rather, if full, the buffer tries to grow and rejects further writes if max capacity is reached.
  *
  * @param initialCapacity the initial buffer size
  * @param maxCapacity the maximum number of elements the buffer can hold.
  */
final private[borer] class ResizableRingBuffer[T](initialCapacity: Int, val maxCapacity: Int) {
  // automatically implies maxCapacity <= 0x40000000
  if (!Util.isPowerOf2(maxCapacity) || maxCapacity <= 0 || !Util.isPowerOf2(
    initialCapacity) || initialCapacity <= 0 || maxCapacity < initialCapacity)
    throw new IllegalArgumentException

  private[this] var array = new Array[AnyRef](initialCapacity)
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

  /**
    * Tries to write the given value into the buffer thereby potentially growing the backing array.
    * Returns `true` if the write was successful and false if the buffer is full and cannot grow anymore.
    */
  def append(value: T): Boolean =
    if (count < currentCapacity) { // if we have space left we can simply write and be done
      val w = writeIx
      array(w & mask) = value.asInstanceOf[AnyRef]
      writeIx = w + 1
      true
    } else grow() && append(value)

  /**
    * Tries to write the given value into the buffer thereby potentially growing the backing array.
    * Returns `true` if the write was successful and false if the buffer is full and cannot grow anymore.
    */
  def prepend(value: T): Boolean =
    if (count < currentCapacity) { // if we have space left we can simply write and be done
      val r = readIx - 1
      array(r & mask) = value.asInstanceOf[AnyRef]
      readIx = r
      true
    } else grow() && prepend(value)

  /**
    * Reads the next value from the buffer.
    * Throws a NoSuchElementException if the buffer is empty.
    */
  def read(): T =
    if (nonEmpty) unsafeRead()
    else throw new NoSuchElementException

  /**
    * Reads the next value from the buffer without any buffer underrun protection.
    */
  def unsafeRead(): T = {
    val r = readIx
    readIx = r + 1
    val ix  = r & mask
    val res = array(ix)
    array(ix) = null
    res.asInstanceOf[T]
  }

  /**
    * Reads the next value from the buffer without any buffer underrun protection
    * and without clearing the reference from the buffer!
    */
  def unsafeRead_NoZero(): T = {
    val r = readIx
    readIx = r + 1
    array(r & mask).asInstanceOf[T]
  }

  def peekNext: T =
    if (nonEmpty) array(readIx & mask).asInstanceOf[T]
    else throw new NoSuchElementException

  def peekFromEnd(offset: Int): T =
    if (nonEmpty) array((writeIx + offset) & mask).asInstanceOf[T]
    else throw new NoSuchElementException

  def dropNext(n: Int): Unit =
    if (n <= count) readIx += n
    else throw new IllegalArgumentException("Cannot drop more elements than currently in the buffer")

  def dropLast(n: Int): Unit =
    if (n <= count) writeIx -= n
    else throw new IllegalArgumentException("Cannot drop more elements than currently in the buffer")

  private def grow(): Boolean =
    (array.length < maxCapacity) && {
      val r        = readIx & mask
      val newArray = new Array[AnyRef](array.length << 1)
      System.arraycopy(array, r, newArray, 0, array.length - r)
      System.arraycopy(array, 0, newArray, array.length - r, r)
      array = newArray
      mask = newArray.length - 1
      writeIx = count
      readIx = 0
      true
    }

  override def toString: String =
    s"ResizableRingBuffer(len=${array.length}, size=$count, writeIx=$writeIx, readIx=$readIx)"
}