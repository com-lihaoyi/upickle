package ujson
import scala.annotation.switch
import upickle.core.{ArrVisitor, ObjVisitor}

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


  private[this] var commaBuffered = false

  def flushBuffer() = {
    if (commaBuffered) {
      commaBuffered = false
      elemBuilder.append(',')
      renderIndent()
    }
  }
  def visitArray(length: Int, index: Int) = new ArrVisitor[T, T] {
    flushBuffer()
    elemBuilder.append('[')

    depth += 1
    renderIndent()
    def subVisitor = BaseElemRenderer.this
    def visitValue(v: T, index: Int): Unit = {
      flushBuffer()
      commaBuffered = true
    }
    def visitEnd(index: Int) = {
      commaBuffered = false
      depth -= 1
      renderIndent()
      elemBuilder.append(']')
      flushElemBuilder()
      out
    }
  }

  def visitObject(length: Int, index: Int) = new ObjVisitor[T, T] {
    flushBuffer()
    elemBuilder.append('{')
    depth += 1
    renderIndent()
    def subVisitor = BaseElemRenderer.this
    def visitKey(index: Int) = BaseElemRenderer.this
    def visitKeyValue(s: Any): Unit = {
      elemBuilder.append(':')
      if (indent != -1) elemBuilder.append(' ')
    }
    def visitValue(v: T, index: Int): Unit = {
      commaBuffered = true
    }
    def visitEnd(index: Int) = {
      commaBuffered = false
      depth -= 1
      renderIndent()
      elemBuilder.append('}')
      flushElemBuilder()
      out
    }
  }

  def visitNull(index: Int) = {
    flushBuffer()
    elemBuilder.ensureLength(4)
    elemBuilder.appendUnsafe('n')
    elemBuilder.appendUnsafe('u')
    elemBuilder.appendUnsafe('l')
    elemBuilder.appendUnsafe('l')
    flushElemBuilder()
    out
  }

  def visitFalse(index: Int) = {
    flushBuffer()
    elemBuilder.ensureLength(5)
    elemBuilder.appendUnsafe('f')
    elemBuilder.appendUnsafe('a')
    elemBuilder.appendUnsafe('l')
    elemBuilder.appendUnsafe('s')
    elemBuilder.appendUnsafe('e')
    flushElemBuilder()
    out
  }

  def visitTrue(index: Int) = {
    flushBuffer()
    elemBuilder.ensureLength(4)
    elemBuilder.appendUnsafe('t')
    elemBuilder.appendUnsafe('r')
    elemBuilder.appendUnsafe('u')
    elemBuilder.appendUnsafe('e')
    flushElemBuilder()
    out
  }

  def visitFloat64StringParts(s: CharSequence, decIndex: Int, expIndex: Int, index: Int) = {
    flushBuffer()
    elemBuilder.ensureLength(s.length())
    var i = 0
    val sLength = s.length
    while(i < sLength){
      elemBuilder.appendUnsafeC(s.charAt(i))
      i += 1
    }
    flushElemBuilder()
    out
  }

  override def visitFloat64(d: Double, index: Int) = {
    d match{
      case Double.PositiveInfinity => visitNonNullString("Infinity", -1)
      case Double.NegativeInfinity => visitNonNullString("-Infinity", -1)
      case d if java.lang.Double.isNaN(d) => visitNonNullString("NaN", -1)
      case d =>
        val i = d.toInt
        if (d == i) visitFloat64StringParts(i.toString, -1, -1, index)
        else super.visitFloat64(d, index)
        flushBuffer()
    }
    flushElemBuilder()
    out
  }

  override def visitFloat32(d: Float, index: Int) = {
    d match{
      case Float.PositiveInfinity => visitNonNullString("Infinity", -1)
      case Float.NegativeInfinity => visitNonNullString("-Infinity", -1)
      case d if java.lang.Float.isNaN(d) => visitNonNullString("NaN", -1)
      case d =>
        val i = d.toInt
        if (d == i) visitFloat64StringParts(i.toString, -1, -1, index)
        else super.visitFloat32(d, index)
        flushBuffer()
    }
    flushElemBuilder()
    out
  }

  def visitString(s: CharSequence, index: Int) = {

    if (s eq null) visitNull(index)
    else visitNonNullString(s, index)
  }

  def visitNonNullString(s: CharSequence, index: Int) = {
    flushBuffer()
    upickle.core.RenderUtils.escapeElem(unicodeCharBuilder, elemBuilder, s, escapeUnicode)
    flushElemBuilder()
    out
  }

  final def renderIndent() = {
    if (indent == -1) ()
    else {
      var i = indent * depth
      elemBuilder.ensureLength(i + 1)
      elemBuilder.appendUnsafe('\n')
      while(i > 0) {
        elemBuilder.appendUnsafe(' ')
        i -= 1
      }
    }
  }
}
