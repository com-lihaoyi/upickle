package ujson

import java.io.ByteArrayOutputStream

import ujson.util.CharBuilder
import upickle.core.{ArrVisitor, ObjVisitor, Visitor}

import scala.annotation.switch

case class BytesRenderer(indent: Int = -1, escapeUnicode: Boolean = false)
  extends BaseRenderer(new BytesRenderer.BytesWriter(), indent, escapeUnicode){
}

object BytesRenderer{
  class BytesWriter(out: java.io.ByteArrayOutputStream = new ByteArrayOutputStream())
    extends java.io.OutputStreamWriter(out){
    def toBytes = {
      this.flush()
      out.toByteArray
    }
  }
}

case class StringRenderer(indent: Int = -1,
                          escapeUnicode: Boolean = false)
  extends BaseRenderer(new java.io.StringWriter(), indent, escapeUnicode)

case class Renderer(out: java.io.Writer,
                    indent: Int = -1,
                    escapeUnicode: Boolean = false)
  extends BaseRenderer[java.io.Writer](out, indent, escapeUnicode)

class BaseRenderer[T <: java.io.Writer]
                  (out: T,
                   indent: Int = -1,
                   escapeUnicode: Boolean = false) extends JsVisitor[T, T]{
  private[this] val charBuilder = new CharBuilder

  def flushCharBuilder() = {
    charBuilder.writeOutToIfLongerThan(out, if (depth == 0) 0 else 1000)
  }

  private[this] var depth: Int = 0


  private[this] var commaBuffered = false

  def flushBuffer() = {
    if (commaBuffered) {
      commaBuffered = false
      charBuilder.append(',')
      renderIndent()
    }
  }
  def visitArray(length: Int, index: Int) = new ArrVisitor[T, T] {
    flushBuffer()
    charBuilder.append('[')

    depth += 1
    renderIndent()
    def subVisitor = BaseRenderer.this
    def visitValue(v: T, index: Int): Unit = {
      flushBuffer()
      commaBuffered = true
    }
    def visitEnd(index: Int) = {
      commaBuffered = false
      depth -= 1
      renderIndent()
      charBuilder.append(']')
      flushCharBuilder()
      out
    }
  }

  def visitObject(length: Int, index: Int) = new ObjVisitor[T, T] {
    flushBuffer()
    charBuilder.append('{')
    depth += 1
    renderIndent()
    def subVisitor = BaseRenderer.this
    def visitKey(index: Int) = BaseRenderer.this
    def visitKeyValue(s: Any): Unit = {
      charBuilder.append(':')
      if (indent != -1) charBuilder.append(' ')
    }
    def visitValue(v: T, index: Int): Unit = {
      commaBuffered = true
    }
    def visitEnd(index: Int) = {
      commaBuffered = false
      depth -= 1
      renderIndent()
      charBuilder.append('}')
      flushCharBuilder()
      out
    }
  }

  def visitNull(index: Int) = {
    flushBuffer()
    charBuilder.ensureLength(4)
    charBuilder.appendUnsafe('n')
    charBuilder.appendUnsafe('u')
    charBuilder.appendUnsafe('l')
    charBuilder.appendUnsafe('l')
    flushCharBuilder()
    out
  }

  def visitFalse(index: Int) = {
    flushBuffer()
    charBuilder.ensureLength(5)
    charBuilder.appendUnsafe('f')
    charBuilder.appendUnsafe('a')
    charBuilder.appendUnsafe('l')
    charBuilder.appendUnsafe('s')
    charBuilder.appendUnsafe('e')
    flushCharBuilder()
    out
  }

  def visitTrue(index: Int) = {
    flushBuffer()
    charBuilder.ensureLength(4)
    charBuilder.appendUnsafe('t')
    charBuilder.appendUnsafe('r')
    charBuilder.appendUnsafe('u')
    charBuilder.appendUnsafe('e')
    flushCharBuilder()
    out
  }

  def visitFloat64StringParts(s: CharSequence, decIndex: Int, expIndex: Int, index: Int) = {
    flushBuffer()
    charBuilder.ensureLength(s.length())
    var i = 0
    val sLength = s.length
    while(i < sLength){
      charBuilder.appendUnsafe(s.charAt(i))
      i += 1
    }
    flushCharBuilder()
    out
  }

  override def visitFloat64(d: Double, index: Int) = {
    d match{
      case Double.PositiveInfinity => visitString("Infinity", -1)
      case Double.NegativeInfinity => visitString("-Infinity", -1)
      case d if java.lang.Double.isNaN(d) => visitString("NaN", -1)
      case d =>
        val i = d.toInt
        if (d == i) visitFloat64StringParts(i.toString, -1, -1, index)
        else super.visitFloat64(d, index)
        flushBuffer()
    }
    flushCharBuilder()
    out
  }


  def visitString(s: CharSequence, index: Int) = {

    if (s eq null) visitNull(index)
    else {
      flushBuffer()
      Renderer.escape(charBuilder, s, escapeUnicode)
      flushCharBuilder()
      out
    }
  }

  final def renderIndent() = {
    if (indent == -1) ()
    else {
      var i = indent * depth
      charBuilder.ensureLength(i + 1)
      charBuilder.appendUnsafe('\n')
      while(i > 0) {
        charBuilder.appendUnsafe(' ')
        i -= 1
      }
    }
  }
}
object Renderer {
  final def escape(sb: ujson.util.CharBuilder, s: CharSequence, unicode: Boolean): Unit = {
    var i = 0
    val len = s.length
    val naiveOutLen = len + 2 // +2 for the start and end quotes
    sb.ensureLength(naiveOutLen)
    sb.appendUnsafe('"')
    while (i < len) {
      (s.charAt(i): @switch) match {
        case '"' => sb.ensureLength(naiveOutLen - i + 1); sb.appendUnsafe('\\'); sb.appendUnsafe('\"')
        case '\\' => sb.ensureLength(naiveOutLen - i + 1); sb.appendUnsafe('\\'); sb.appendUnsafe('\\')
        case '\b' => sb.ensureLength(naiveOutLen - i + 1); sb.appendUnsafe('\\'); sb.appendUnsafe('b')
        case '\f' => sb.ensureLength(naiveOutLen - i + 1); sb.appendUnsafe('\\'); sb.appendUnsafe('f')
        case '\n' => sb.ensureLength(naiveOutLen - i + 1); sb.appendUnsafe('\\'); sb.appendUnsafe('n')
        case '\r' => sb.ensureLength(naiveOutLen - i + 1); sb.appendUnsafe('\\'); sb.appendUnsafe('r')
        case '\t' => sb.ensureLength(naiveOutLen - i + 1); sb.appendUnsafe('\\'); sb.appendUnsafe('t')
        case c =>
          if (c < ' ' || (c > '~' && unicode)) {
            sb.ensureLength(naiveOutLen - i + 4);
            sb.appendUnsafe('\\')
            sb.appendUnsafe('u')
            sb.appendUnsafe(toHex((c >> 12) & 15))
            sb.appendUnsafe(toHex((c >> 8) & 15))
            sb.appendUnsafe(toHex((c >> 4) & 15))
            sb.appendUnsafe(toHex(c & 15))
          } else sb.append(c)
      }
      i += 1
    }
    sb.appendUnsafe('"')
  }

  private def toHex(nibble: Int): Char = (nibble + (if (nibble >= 10) 87 else 48)).toChar
}