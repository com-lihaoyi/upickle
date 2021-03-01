package ujson.borer


import java.nio.ByteBuffer

/**
  * Type class for providing basic access to a `Bytes` abstraction,
  * as well as construction of a respective [[Output]].
  */
trait ByteAccess[Bytes] {
  type Out <: Output

  /**
    * Returns an empty [[Bytes]] instance.
    */
  def empty: Bytes

  /**
    * Returns true iff the given [[Bytes]] instance is empty.
    */
  def isEmpty(bytes: Bytes): Boolean

  /**
    * Returns the number of bytes contained in the given [[Bytes]] instance.
    */
  def sizeOf(bytes: Bytes): Long

  /**
    * Converts the given byte array into a [[Bytes]] instance.
    */
  def fromByteArray(byteArray: Array[Byte]): Bytes

  /**
    * Converts the given [[Bytes]] instance into a byte array.
    */
  def toByteArray(bytes: Bytes): Array[Byte]

  /**
    * Copies the given [[Bytes]] instance into the given `byteArray` starting at the given index.
    * Returns a [[Bytes]] instance holding all bytes that could not be written to the byte array
    * due to capacity constraints or an empty [[Bytes]] instance, if the given byte array was
    * large enough to hold all `bytes`.
    */
  def copyToByteArray(bytes: Bytes, byteArray: Array[Byte], startIndex: Int): Bytes

  /**
    * Copies the given [[Bytes]] instance into the given `byteBuffer`.
    * Returns a [[Bytes]] instance holding all bytes that could not be written to the byteBuffer
    * due to capacity constraints or an empty [[Bytes]] instance, if the given byteBuffer was
    * large enough to hold all `bytes`.
    */
  def copyToByteBuffer(bytes: Bytes, byteBuffer: ByteBuffer): Bytes

  /**
    * Returns the concatenation of `a` and `b`.
    */
  def concat(a: Bytes, b: Bytes): Bytes

  /**
    * Converts the given `value` of type [[B]] into a [[Bytes]] instance.
    */
  def convert[B: ByteAccess](value: B): Bytes
}

object ByteAccess {

  /**
    * The default [[ByteAccess]] for plain byte arrays.
    */
  implicit final object ForByteArray extends ByteAccess[Array[Byte]] {
    type Out = Output.ToByteArray

    @inline def empty = Array.emptyByteArray

    @inline def isEmpty(bytes: Array[Byte]) = bytes.length == 0

    @inline def sizeOf(bytes: Array[Byte]): Long = bytes.length.toLong

    @inline def fromByteArray(byteArray: Array[Byte]): Array[Byte] = byteArray

    @inline def toByteArray(bytes: Array[Byte]): Array[Byte] = bytes

    def copyToByteArray(bytes: Array[Byte], byteArray: Array[Byte], startIndex: Int): Array[Byte] = {
      val dstRemaining = byteArray.length - startIndex
      if (bytes.length <= dstRemaining) {
        System.arraycopy(bytes, 0, byteArray, startIndex, bytes.length)
        empty
      } else {
        val rest = new Array[Byte](bytes.length - dstRemaining)
        System.arraycopy(bytes, 0, byteArray, startIndex, dstRemaining)
        System.arraycopy(bytes, dstRemaining, rest, 0, rest.length)
        rest
      }
    }

    def copyToByteBuffer(bytes: Array[Byte], byteBuffer: ByteBuffer): Array[Byte] = {
      val dstRemaining = byteBuffer.remaining()
      if (bytes.length <= dstRemaining) {
        byteBuffer.put(bytes)
        empty
      } else {
        val rest = new Array[Byte](bytes.length - dstRemaining)
        byteBuffer.put(bytes, 0, dstRemaining)
        System.arraycopy(bytes, dstRemaining, rest, 0, rest.length)
        rest
      }
    }

    def concat(a: Array[Byte], b: Array[Byte]): Array[Byte] =
      if (a.length > 0) {
        if (b.length > 0) {
          val len = a.length + b.length
          if (len >= 0) {
            val result = new Array[Byte](len)
            System.arraycopy(a, 0, result, 0, a.length)
            System.arraycopy(b, 0, result, a.length, b.length)
            result
          } else sys.error("Cannot concatenate two byte arrays with a total size > 2^31 bytes")
        } else a
      } else b

    @inline def convert[B](value: B)(implicit byteAccess: ByteAccess[B]): Array[Byte] =
      value match {
        case x: Array[Byte] => x
        case x              => byteAccess.toByteArray(x)
      }
  }

  /**
    * The default [[ByteAccess]] for [[ByteBuffer]].
    */
  implicit final object ForByteBuffer extends ByteAccess[ByteBuffer] {
    type Out = Output.ToByteBuffer

    val empty = ByteBuffer.wrap(Array.emptyByteArray)

    def isEmpty(bytes: ByteBuffer) = !bytes.hasRemaining

    @inline def sizeOf(bytes: ByteBuffer): Long = bytes.remaining.toLong

    @inline def fromByteArray(byteArray: Array[Byte]): ByteBuffer = ByteBuffer.wrap(byteArray)

    @inline def toByteArray(bytes: ByteBuffer): Array[Byte] = {
      val byteArray = new Array[Byte](bytes.remaining)
      bytes.mark()
      bytes.get(byteArray, 0, byteArray.length)
      bytes.reset()
      byteArray
    }

    def copyToByteArray(bytes: ByteBuffer, byteArray: Array[Byte], startIndex: Int): ByteBuffer = {
      bytes.mark()
      bytes.get(byteArray, startIndex, byteArray.length - startIndex)
      bytes.reset()
      bytes
    }

    def copyToByteBuffer(bytes: ByteBuffer, byteBuffer: ByteBuffer): ByteBuffer = {
      val srcRemaining = bytes.remaining
      val dstRemaining = byteBuffer.remaining
      if (srcRemaining <= dstRemaining) {
        bytes.mark()
        byteBuffer.put(bytes)
        bytes.reset()
      } else byteBuffer.put(bytes.slice.limit(dstRemaining).asInstanceOf[ByteBuffer])
      bytes
    }

    def concat(a: ByteBuffer, b: ByteBuffer): ByteBuffer =
      if (a.hasRemaining) {
        if (b.hasRemaining) {
          val len = a.remaining + b.remaining
          if (len >= 0) {
            val result = ByteBuffer.allocate(len)
            a.mark()
            result.put(a)
            a.reset()
            b.mark()
            result.put(b)
            b.reset()
            result.flip()
            result
          } else sys.error("Cannot concatenate two ByteBuffers with a total size > 2^31 bytes")
        } else a
      } else b

    @inline def convert[B](value: B)(implicit byteAccess: ByteAccess[B]): ByteBuffer =
      value match {
        case x: ByteBuffer => x
        case x             => fromByteArray(byteAccess.toByteArray(x))
      }
  }
}