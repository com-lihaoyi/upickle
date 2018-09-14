package ujson

import java.io.ByteArrayOutputStream



import scala.annotation.switch

case class BytesRenderer(indent: Int = -1)
  extends BaseRenderer(new BytesRenderer.BytesWriter(), indent){
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
case class StringRenderer(indent: Int = -1)
  extends BaseRenderer(new java.io.StringWriter(), indent)

case class Renderer(out: java.io.Writer,
                    indent: Int = -1) extends BaseRenderer[java.io.Writer](out, indent)

class BaseRenderer[T <: java.io.Writer]
                  (out: T,
                   indent: Int = -1) extends ujson.Visitor[T, T]{
  var depth: Int = 0
  val colonSnippet = if (indent == -1) ":" else ": "

  var commaBuffered = false

  def flushBuffer() = {
    if (commaBuffered) {
      commaBuffered = false
      out.append(',')
      renderIndent()
    }
  }
  def visitArray(index: Int) = new ArrVisitor[T, T] {
    flushBuffer()
    out.append('[')

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
      out.append(']')
      out
    }
  }

  def visitObject(index: Int) = new ObjVisitor[T, T] {
    flushBuffer()
    out.append('{')
    depth += 1
    renderIndent()
    def subVisitor = BaseRenderer.this
    def visitKey(s: CharSequence, index: Int): Unit = {
      flushBuffer()

      Renderer.escape(out, s, true)

      out.append(colonSnippet)
    }
    def visitValue(v: T, index: Int): Unit = {
      commaBuffered = true
    }
    def visitEnd(index: Int) = {
      commaBuffered = false
      depth -= 1
      renderIndent()
      out.append('}')
      out
    }
  }

  def visitNull(index: Int) = {
    flushBuffer()
    out.append("null")
    out
  }

  def visitFalse(index: Int) = {
    flushBuffer()
    out.append("false")
    out
  }

  def visitTrue(index: Int) = {
    flushBuffer()
    out.append("true")
    out
  }

  def visitNum(s: CharSequence, decIndex: Int, expIndex: Int, index: Int) = {
    flushBuffer()
    out.append(s)
    out
  }

  override def visitNumRaw(d: Double, index: Int) = {
    val i = d.toInt
    if (d == i) visitNum(i.toString, -1, -1, index)
    else super.visitNumRaw(d, index)
    flushBuffer()
    out
  }

  def visitString(s: CharSequence, index: Int) = {
    flushBuffer()
    if (s == null) out.append("null")
    else Renderer.escape(out, s, true)

    out
  }

  final def renderIndent() = {
    if (indent == -1) ()
    else {
      out.append('\n')
      var i = indent * depth
      while(i > 0) {
        out.append(' ')
        i -= 1
      }
    }
  }
}
object Renderer {
  final def escape(sb: java.io.Writer, s: CharSequence, unicode: Boolean): Unit = {
    sb.append('"')
    var i = 0
    val len = s.length
    while (i < len) {
      (s.charAt(i): @switch) match {
        case '"' => sb.append("\\\"")
        case '\\' => sb.append("\\\\")
        case '\b' => sb.append("\\b")
        case '\f' => sb.append("\\f")
        case '\n' => sb.append("\\n")
        case '\r' => sb.append("\\r")
        case '\t' => sb.append("\\t")
        case c =>
          if (c < ' ' || (c > '~' && unicode)) {
            sb.append("\\u").append(toHex((c >> 12) & 15)).append(toHex((c >> 8) & 15))
              .append(toHex((c >> 4) & 15)).append(toHex(c & 15))
          } else sb.append(c)
      }
      i += 1
    }
    sb.append('"')
  }

  private def toHex(nibble: Int): Char = (nibble + (if (nibble >= 10) 87 else 48)).toChar
}