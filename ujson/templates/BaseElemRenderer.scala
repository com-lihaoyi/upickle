package ujson
import scala.annotation.switch
import upickle.core.{ArrVisitor, ObjVisitor, ElemBuilder, RenderUtils}

/**
  * A specialized JSON renderer that can render Elems (Chars or Bytes) directly
  * to a [[java.io.Writer]] or [[java.io.OutputStream]]
  *
  * Note that we use an internal `ElemBuilder` to buffer the output internally
  * before sending it to [[out]] in batches. This lets us benefit from the high
  * performance and minimal overhead of `ElemBuilder` in the fast path of
  * pushing characters, and avoid the synchronization/polymorphism overhead of
  * [[out]] on the fast path. Most [[out]]s would also have performance
  * benefits from receiving data in batches, rather than elem by elem.
  */
class BaseElemRenderer[T <: upickle.core.ElemOps.Output]
                      (out: T,
                       indent: Int = -1,
                       escapeUnicode: Boolean = false) extends JsVisitor[T, T]{
  private[this] val elemBuilder = new upickle.core.ElemBuilder
  private[this] val unicodeCharBuilder = new upickle.core.CharBuilder()
  def flushElemBuilder() = {
    elemBuilder.writeOutToIfLongerThan(out, if (depth == 0) 0 else 1000)
  }

  private[this] var depth: Int = 0

  private[this] var visitingKey = false

  private[this] var commaBuffered = false
  private[this] var indentBuffered = false
  private[this] var quoteBuffered = false

  def flushBuffer() = {
    if (commaBuffered) {
      commaBuffered = false
      elemBuilder.append(',')
    }
    if (indentBuffered){
      indentBuffered = false
      renderIndent()
    }
    if (quoteBuffered) {
      quoteBuffered = false
      elemBuilder.append('"')
    }
  }

  def visitArray(length: Int, index: Int) = new ArrVisitor[T, T] {
    flushBuffer()
    elemBuilder.append('[')

    depth += 1
    indentBuffered = true

    def subVisitor = BaseElemRenderer.this

    def visitValue(v: T, index: Int): Unit = {
      flushBuffer()
      commaBuffered = true
      indentBuffered = true
    }

    def visitEnd(index: Int) = {
      depth -= 1
      if (indentBuffered && commaBuffered) renderIndent()
      commaBuffered = false
      indentBuffered = false
      elemBuilder.append(']')
      flushElemBuilder()
      out
    }
  }

  def visitJsonableObject(length: Int, index: Int) = new ObjVisitor[T, T] {
    flushBuffer()
    elemBuilder.append('{')
    depth += 1
    indentBuffered = true

    def subVisitor = BaseElemRenderer.this
    def visitKey(index: Int) = {
      quoteBuffered = true
      visitingKey = true
      BaseElemRenderer.this
    }

    def visitKeyValue(s: Any): Unit = {
      elemBuilder.append('"')
      visitingKey = false
      elemBuilder.append(':')
      if (indent != -1) elemBuilder.append(' ')
    }

    def visitValue(v: T, index: Int): Unit = {
      commaBuffered = true
      indentBuffered = true
    }

    def visitEnd(index: Int) = {
      depth -= 1
      if (indentBuffered && commaBuffered) renderIndent()
      commaBuffered = false
      indentBuffered = false
      elemBuilder.append('}')
      flushElemBuilder()
      out
    }
  }

  def visitNull(index: Int) = {
    flushBuffer()
    BaseElemRenderer.appendNull(elemBuilder)
    flushElemBuilder()
    out
  }

  def visitFalse(index: Int) = {
    flushBuffer()
    BaseElemRenderer.appendFalse(elemBuilder)
    flushElemBuilder()
    out
  }

  def visitTrue(index: Int) = {
    flushBuffer()
    BaseElemRenderer.appendTrue(elemBuilder)
    flushElemBuilder()
    out
  }

  def visitFloat64StringParts(s: CharSequence, decIndex: Int, expIndex: Int, index: Int) = {
    flushBuffer()
    BaseElemRenderer.appendKnownAsciiString(elemBuilder, s)
    flushElemBuilder()
    out
  }

  override def visitFloat32(d: Float, index: Int) = {
    d match{
      case Float.PositiveInfinity => visitNonNullString("Infinity", -1)
      case Float.NegativeInfinity => visitNonNullString("-Infinity", -1)
      case d if java.lang.Float.isNaN(d) => visitNonNullString("NaN", -1)
      case d =>
        // Ensure that for whole numbers that can be exactly represented by an
        // int or long, write them in int notation with decimal points or exponents
        val i = d.toInt
        if (d == i) visitInt32(i, index)
        else {
          val i = d.toLong
          flushBuffer()
          if (i == d) BaseElemRenderer.appendKnownAsciiString(elemBuilder, d.toString)
          else {
            elemBuilder.ensureLength(15)
            elemBuilder.length += ujson.FloatToDecimalElem.toString(elemBuilder.arr, elemBuilder.length, d)
          }
          flushElemBuilder()
        }
    }
    out
  }

  override def visitFloat64(d: Double, index: Int) = {
    d match{
      case Double.PositiveInfinity => visitNonNullString("Infinity", -1)
      case Double.NegativeInfinity => visitNonNullString("-Infinity", -1)
      case d if java.lang.Double.isNaN(d) => visitNonNullString("NaN", -1)
      case d =>
        // Ensure that for whole numbers that can be exactly represented by an
        // int or long, write them in int notation with decimal points or exponents
        val i = d.toInt
        if (d == i) visitInt32(i, index)
        else {
          val i = d.toLong
          flushBuffer()
          if (i == d) BaseElemRenderer.appendKnownAsciiString(elemBuilder, i.toString)
          else {
            elemBuilder.ensureLength(24)
            elemBuilder.length += ujson.DoubleToDecimalElem.toString(elemBuilder.arr, elemBuilder.length, d)
          }
          flushElemBuilder()
        }
    }
    out
  }

  override def visitInt32(i: Int, index: Int) = {
    flushBuffer()
    BaseElemRenderer.appendIntString(elemBuilder, i)
    flushElemBuilder()
    out
  }

  override def visitInt64(i: Long, index: Int) = {
    flushBuffer()
    if (math.abs(i) > 9007199254740992L /*math.pow(2, 53)*/ ||
        i == -9223372036854775808L /*Long.MinValue*/ ) {
      elemBuilder.append('"')
      BaseElemRenderer.appendLongString(elemBuilder, i)
      elemBuilder.append('"')
    } else BaseElemRenderer.appendLongString(elemBuilder, i)
    flushElemBuilder()
    out
  }

  override def visitUInt64(i: Long, index: Int) = {
    val int = i.toInt
    if (int == i) visitInt32(int, index)
    else super.visitUInt64(i, index)
    out
  }

  def visitString(s: CharSequence, index: Int) = {

    if (s eq null) visitNull(index)
    else visitNonNullString(s, index)
  }

  def visitNonNullString(s: CharSequence, index: Int) = {
    flushBuffer()

    upickle.core.RenderUtils.escapeElem(
      unicodeCharBuilder, elemBuilder, s, escapeUnicode, !visitingKey
    )

    flushElemBuilder()
    out
  }

  final def renderIndent() = {
    if (indent == -1) ()
    else {
      var i = indent * depth
      elemBuilder.ensureLength(i + 1)
      BaseElemRenderer.renderIdent(elemBuilder.arr, elemBuilder.length, i)
      elemBuilder.length += i + 1
    }
  }
}

object BaseElemRenderer{
  private def renderIdent(arr: Array[Elem], length: Int, i0: Int) = {
    var i = i0
    arr(length) = '\n'
    while (i > 0) {
      arr(length + i) = ' '
      i -= 1
    }
  }

  private def appendIntString(eb: ElemBuilder, i0: Int) = {
    val size = RenderUtils.intStringSize(i0)
    val newLength = eb.length + size
    eb.ensureLength(size)
    appendIntString0(i0, newLength, eb.arr)
    eb.length = newLength
  }

  private def appendIntString0(i0: Int, index: Int, arr: Array[Elem]) = {
    // Copied from java.lang.Integer.getChars
    var i = i0
    var q = 0
    var r = 0
    var charPos = index
    val negative = i < 0
    if (!negative) i = -i
    // Generate two digits per iteration
    while (i <= -100) {
      q = i / 100
      r = (q * 100) - i
      i = q
      charPos -= 1
      arr(charPos) = DigitOnes(r)
      charPos -= 1
      arr(charPos) = DigitTens(r)
    }
    // We know there are at most two digits left at this point.
    q = i / 10
    r = (q * 10) - i
    charPos -= 1
    arr(charPos) = ('0' + r).toElem
    // Whatever left is the remaining digit.
    if (q < 0) {
      charPos -= 1
      arr(charPos) = ('0' - q).toElem
    }
    if (negative) {
      charPos -= 1;
      arr(charPos) = '-'.toElem
    }
    charPos
  }

  private def appendLongString(eb: ElemBuilder, i0: Long) = {
    val size = RenderUtils.longStringSize(i0)
    val newLength = eb.length + size
    eb.ensureLength(size)
    appendLongString0(i0, newLength, eb.arr)
    eb.length = newLength
  }

  private def appendLongString0(i0: Long, index: Int, buf: Array[Elem]) = {
    // Copied from java.lang.Long.getChars
    var i = i0
    var q = 0L
    var r = 0
    var charPos = index
    val negative = i < 0
    if (!negative) i = -i
    // Get 2 digits/iteration using longs until quotient fits into an int
    while (i <= Integer.MIN_VALUE) {
      q = i / 100
      r = ((q * 100) - i).toInt
      i = q
      charPos -= 1
      buf(charPos) = DigitOnes(r)
      charPos -= 1
      buf(charPos) = DigitTens(r)
    }
    // Get 2 digits/iteration using ints
    var q2 = 0
    var i2 = i.toInt
    while (i2 <= -100) {
      q2 = i2 / 100
      r = (q2 * 100) - i2
      i2 = q2
      charPos -= 1;
      buf(charPos) = DigitOnes(r)
      charPos -= 1;
      buf(charPos) = DigitTens(r)
    }
    // We know there are at most two digits left at this point.
    q2 = i2 / 10
    r = (q2 * 10) - i2
    charPos -= 1
    buf(charPos) = ('0' + r).toElem
    // Whatever left is the remaining digit.
    if (q2 < 0) {
      charPos -= 1
      buf(charPos) = ('0' - q2).toElem
    }
    if (negative) {
      charPos -= 1
      buf(charPos) = '-'.toElem
    }
    charPos
  }

  private val DigitTens = Array[Elem](
    '0', '0', '0', '0', '0', '0', '0', '0', '0', '0',
    '1', '1', '1', '1', '1', '1', '1', '1', '1', '1',
    '2', '2', '2', '2', '2', '2', '2', '2', '2', '2',
    '3', '3', '3', '3', '3', '3', '3', '3', '3', '3',
    '4', '4', '4', '4', '4', '4', '4', '4', '4', '4',
    '5', '5', '5', '5', '5', '5', '5', '5', '5', '5',
    '6', '6', '6', '6', '6', '6', '6', '6', '6', '6',
    '7', '7', '7', '7', '7', '7', '7', '7', '7', '7',
    '8', '8', '8', '8', '8', '8', '8', '8', '8', '8',
    '9', '9', '9', '9', '9', '9', '9', '9', '9', '9',
  )

  private val DigitOnes = Array[Elem](
    '0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
    '0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
    '0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
    '0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
    '0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
    '0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
    '0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
    '0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
    '0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
    '0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
  )

  private def appendNull(eb: ElemBuilder) = {
    eb.ensureLength(4)
    appendNull0(eb.arr, eb.length)
    eb.length += 4
  }

  private def appendNull0(arr: Array[Elem], arrOffset: Int) = {
    arr(arrOffset) = 'n'.toElem
    arr(arrOffset + 1) = 'u'.toElem
    arr(arrOffset + 2) = 'l'.toElem
    arr(arrOffset + 3) = 'l'.toElem
  }

  private def appendTrue(eb: ElemBuilder) = {
    eb.ensureLength(4)
    appendTrue0(eb.arr, eb.length)
    eb.length += 4
  }

  private def appendTrue0(arr: Array[Elem], arrOffset: Int) = {
    arr(arrOffset) = 't'.toElem
    arr(arrOffset + 1) = 'r'.toElem
    arr(arrOffset + 2) = 'u'.toElem
    arr(arrOffset + 3) = 'e'.toElem
  }

  private def appendFalse(eb: ElemBuilder) = {
    eb.ensureLength(5)
    appendFalse0(eb.arr, eb.length)
    eb.length += 5
  }

  private def appendFalse0(arr: Array[Elem], arrOffset: Int) = {
    arr(arrOffset) = 'f'.toElem
    arr(arrOffset + 1) = 'a'.toElem
    arr(arrOffset + 2) = 'l'.toElem
    arr(arrOffset + 3) = 's'.toElem
    arr(arrOffset + 4) = 'e'.toElem
  }

  private def appendKnownAsciiString(eb: ElemBuilder, s: CharSequence) = {
    val sLength = s.length
    eb.ensureLength(sLength)
    appendKnownAsciiString0(eb.arr, eb.length, s, sLength)

    eb.length += sLength
  }

  private def appendKnownAsciiString0(arr: Array[Elem], arrOffset: Int, s: CharSequence, sLength: Int) = {
    var i = 0
    while (i < sLength) {
      arr(arrOffset + i) = s.charAt(i).toElem
      i += 1
    }
  }
}