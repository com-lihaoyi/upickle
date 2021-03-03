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

  def flushIfEnd() = {
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
      flushIfEnd()
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
      flushIfEnd()
      out
    }
  }

  def visitNull(index: Int) = {
    flushBuffer()
    charBuilder.incrementLength(4)
    charBuilder.appendUnsafe('n')
    charBuilder.appendUnsafe('u')
    charBuilder.appendUnsafe('l')
    charBuilder.appendUnsafe('l')
    flushIfEnd()
    out
  }

  def visitFalse(index: Int) = {
    flushBuffer()
    charBuilder.incrementLength(5)
    charBuilder.appendUnsafe('f')
    charBuilder.appendUnsafe('a')
    charBuilder.appendUnsafe('l')
    charBuilder.appendUnsafe('s')
    charBuilder.appendUnsafe('e')
    flushIfEnd()
    out
  }

  def visitTrue(index: Int) = {
    flushBuffer()
    charBuilder.incrementLength(4)
    charBuilder.appendUnsafe('t')
    charBuilder.appendUnsafe('r')
    charBuilder.appendUnsafe('u')
    charBuilder.appendUnsafe('e')
    flushIfEnd()
    out
  }

  def visitFloat64StringParts(s: CharSequence, decIndex: Int, expIndex: Int, index: Int) = {
    flushBuffer()
    charBuilder.incrementLength(s.length())
    var i = 0
    val sLength = s.length
    while(i < sLength){
      charBuilder.appendUnsafe(s.charAt(i))
      i += 1
    }
    flushIfEnd()
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
    flushIfEnd()
    out
  }


  def visitString(s: CharSequence, index: Int) = {

    if (s eq null) visitNull(index)
    else {
      flushBuffer()
      Renderer.escape(charBuilder, s, escapeUnicode)
      flushIfEnd()
      out
    }
  }

  final def renderIndent() = {
    if (indent == -1) ()
    else {
      var i = indent * depth
      charBuilder.incrementLength(i + 1)
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
    sb.append('"')
    var i = 0
    val len = s.length
    while (i < len) {
      (s.charAt(i): @switch) match {
        case '"' => sb.append('\\'); sb.append('\"')
        case '\\' => sb.append('\\'); sb.append('\\')
        case '\b' => sb.append('\\'); sb.append('b')
        case '\f' => sb.append('\\'); sb.append('f')
        case '\n' => sb.append('\\'); sb.append('n')
        case '\r' => sb.append('\\'); sb.append('r')
        case '\t' => sb.append('\\'); sb.append('t')
        case c =>
          if (c < ' ' || (c > '~' && unicode)) {
            sb.append('\\')
            sb.append('u')
            sb.append(toHex((c >> 12) & 15))
            sb.append(toHex((c >> 8) & 15))
            sb.append(toHex((c >> 4) & 15))
            sb.append(toHex(c & 15))
          } else sb.append(c)
      }
      i += 1
    }
    sb.append('"')
  }

  private def toHex(nibble: Int): Char = (nibble + (if (nibble >= 10) 87 else 48)).toChar
}