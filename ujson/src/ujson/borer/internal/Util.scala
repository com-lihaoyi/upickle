package ujson.borer.internal


import scala.annotation.tailrec
import scala.reflect.ClassTag
import scala.util.control.NonFatal

private[borer] object Util {

  val isJS  = 1.0.toString == "1"
  val isJVM = !isJS

  // "platform-independent" toString for Doubles, appends a ".0" suffix on JS, if required
  def floatToString(value: Float): String = fixFloatingPointNumbersOnJS(java.lang.Float.toString(value))

  // "platform-independent" toString for Doubles, appends a ".0" suffix on JS, if required
  def doubleToString(value: Double): String = fixFloatingPointNumbersOnJS(java.lang.Double.toString(value))

  private def fixFloatingPointNumbersOnJS(s: String): String = {
    // check, whether the string consists only of digits (except for the first char, which might be a minus sign)
    @tailrec def onlyDigits(ix: Int): Boolean = ix <= 0 || { val c = s(ix); '0' <= c && c <= '9' && onlyDigits(ix - 1) }
    if (isJS && onlyDigits(s.length - 1)) s + ".0" else s
  }

  @inline def requireNonNegative(value: Int, name: String): Int = requireNonNegative(value.toLong, name).toInt

  @inline def requireNonNegative(value: Long, name: String): Long =
    if (value < 0) throw new IllegalArgumentException(s"$name must be >= 0, but was $value")
    else value

  @inline def requirePositive(value: Int, name: String): Int = requirePositive(value.toLong, name).toInt

  @inline def requirePositive(value: Long, name: String): Long =
    if (value <= 0) throw new IllegalArgumentException(s"$name must be > 0, but was $value")
    else value

  @inline def requireRange(value: Int, min: Int, max: Int, name: String): Int =
    requireRange(value.toLong, min.toLong, max.toLong, name).toInt

  @inline def requireRange(value: Long, min: Long, max: Long, name: String): Long =
    if (min <= value && value <= max) value
    else throw new IllegalArgumentException(s"$name must be in the range [$min, $max], but was $value")

  def isPowerOf2(i: Int): Boolean = Integer.lowestOneBit(i) == i

  private[this] val _identityFunc = (x: Any) => x
  def identityFunc[T]: T => T     = _identityFunc.asInstanceOf[T => T]

  @inline def isChar(x: Int): Boolean              = (x >> 16) == 0
  @inline def isByte(x: Int): Boolean              = (x >> 7) == (x >> 31)
  @inline def isShort(x: Int): Boolean             = (x >> 15) == (x >> 31)
  @inline def isInt(x: Long): Boolean              = (x >> 31) == (x >> 63)
  @inline def isUnsignedInt(uLong: Long): Boolean  = uLong >> 31 == 0
  @inline def isUnsignedLong(uLong: Long): Boolean = uLong >= 0

  def emptyArray[T](implicit ct: ClassTag[T]): Array[T] =
    (ct.runtimeClass match {
      case java.lang.Byte.TYPE      => Array.emptyByteArray
      case java.lang.Short.TYPE     => Array.emptyShortArray
      case java.lang.Character.TYPE => Array.emptyCharArray
      case java.lang.Integer.TYPE   => Array.emptyIntArray
      case java.lang.Long.TYPE      => Array.emptyLongArray
      case java.lang.Float.TYPE     => Array.emptyFloatArray
      case java.lang.Double.TYPE    => Array.emptyDoubleArray
      case java.lang.Boolean.TYPE   => Array.emptyBooleanArray
      case _                        => java.lang.reflect.Array.newInstance(ct.runtimeClass, 0)
    }).asInstanceOf[Array[T]]

  def toBigEndianBytes(uLong: Long): Array[Byte] = {
    val bytes = new Array[Byte](8)
    bytes(0) = (uLong >>> 56).toByte
    bytes(1) = (uLong >>> 48).toByte
    bytes(2) = (uLong >>> 40).toByte
    bytes(3) = (uLong >>> 32).toByte
    bytes(4) = (uLong >>> 24).toByte
    bytes(5) = (uLong >>> 16).toByte
    bytes(6) = (uLong >>> 8).toByte
    bytes(7) = uLong.toByte
    bytes
  }

  def canBeRepresentedAsFloat16(value: Float): Boolean = {
    val bits = java.lang.Float.floatToIntBits(value)
    // Float has 23 mantissa bits, Float16 has only 10
    // so the 13 lower bits of the mantissa must be zero
    (bits & ((1 << 13) - 1)) == 0 && { // are the 13 lower bits of the mantissa zero?
      val exp = (bits << 1) >>> 24 // move out sign bit and get 8 bit exponent
      exp == 0 || exp == 0xFF || { // is exp a special value?
        val normalizedExp = exp - 127 // reverse exponent bias
        (normalizedExp >> 4) == (normalizedExp >> 31) // does normalizedExp fit into 5 bits?
      }
    }
  }

  @inline def canBeRepresentedAsFloat(value: Double): Boolean = value.isNaN || value.toFloat.toDouble == value

  def inPlaceNegate(bytes: Array[Byte]): Unit = {
    @tailrec def rec(ix: Int): Unit =
      if (ix < bytes.length) {
        bytes(ix) = (~bytes(ix).toInt).toByte; rec(ix + 1)
      }
    rec(0)
  }

  /**
    * Returns a positive number if the first `charsLen` characters of `chars` compare greater than `string`,
    * zero if equal, and a negative number if less.
    */
  def charsStringCompare(chars: Array[Char], charsLen: Int, string: String): Int = {
    val limit = math.min(charsLen, string.length)
    @tailrec def rec(ix: Int): Int =
      if (ix < limit) {
        val diff = chars(ix).toInt - string.charAt(ix).toInt
        if (diff != 0) diff else rec(ix + 1)
      } else charsLen - string.length
    rec(0)
  }

  /**
    * Returns a positive number if the first `aLen` characters of `a` compare greater than `b`,
    * zero if equal, and a negative number if less.
    */
  def charsCharsCompare(a: Array[Char], aLen: Int, b: Array[Char]): Int = {
    val limit = math.min(aLen, b.length)
    @tailrec def rec(ix: Int): Int =
      if (ix < limit) {
        val diff = a(ix).toInt - b(ix).toInt
        if (diff != 0) diff else rec(ix + 1)
      } else aLen - b.length
    rec(0)
  }

  implicit final class RichIterator[T](val underlying: Iterator[T]) extends AnyVal {

    def +:(element: T): Iterator[T] =
      new Iterator[T] {
        private[this] var pending = true
        def hasNext               = pending || underlying.hasNext

        def next() =
          if (pending) {
            pending = false
            element
          } else underlying.next()
      }
  }

  implicit class StringInterpolators(val sc: StringContext) extends AnyVal {

    def hex(args: Any*): Array[Byte] = {
      val hexString = sc.s(args: _*)
      try {
        if ((hexString.length & 1) != 0) sys.error("string length is not even")
        hexString.grouped(2).map(Integer.parseInt(_, 16).toByte).toArray
      } catch {
        case NonFatal(e) =>
          throw new IllegalArgumentException(s"`$hexString` is not a valid hex string", e)
      }
    }
  }
}