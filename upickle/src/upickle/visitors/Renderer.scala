package upickle
package visitors

import java.io.{ByteArrayOutputStream, StringWriter, Writer}

import upickle.jawn.{ArrVisitor, ObjArrVisitor, ObjVisitor, Visitor}

import scala.annotation.switch
class StringRenderer(indent: Int = -1,
                     depth: Int = 0) extends Visitor[StringWriter, StringWriter]{// extends Renderer(new StringWriter(), indent, depth){
  def visitArray(index: Int) = ???

  def visitObject(index: Int) = ???

  def visitNull(index: Int) = ???

  def visitFalse(index: Int) = ???

  def visitTrue(index: Int) = ???

  def visitNum(s: CharSequence, decIndex: Int, expIndex: Int, index: Int) = ???

  def visitString(s: CharSequence, index: Int) = ???
}
class Renderer(out: java.io.Writer,
               var indent: Int = -1,
               var depth: Int = 0) extends upickle.jawn.Visitor[Unit, Unit]{
  val colonSnippet = if (indent == -1) ":" else ": "

  var commaBuffered = false

  def flushBuffer() = {
    if (commaBuffered) {
      commaBuffered = false
      out.append(',')
      renderIndent()
    }
  }
  def visitArray(index: Int) = new ArrVisitor[Unit, Unit] {
    flushBuffer()
    out.append('[')

    depth += 1
    renderIndent()
    def subVisitor = Renderer.this
    def visitValue(v: Unit, index: Int): Unit = {
      flushBuffer()
      commaBuffered = true
    }
    def visitEnd(index: Int): Unit = {
      commaBuffered = false
      depth -= 1
      renderIndent()
      out.append(']')
    }
  }

  def visitObject(index: Int) = new ObjVisitor[Unit, Unit] {
    flushBuffer()
    out.append('{')
    depth += 1
    renderIndent()
    def subVisitor = Renderer.this
    def visitKey(s: CharSequence, index: Int): Unit = {
      flushBuffer()

      Renderer.escape(out, s, true)

      out.append(colonSnippet)
    }
    def visitValue(v: Unit, index: Int): Unit = {
      commaBuffered = true
    }
    def visitEnd(index: Int): Unit = {
      commaBuffered = false
      depth -= 1
      renderIndent()
      out.append('}')
    }
  }

  def visitNull(index: Int) = {
    flushBuffer()
    out.append("null")
  }

  def visitFalse(index: Int) = {
    flushBuffer()
    out.append("false")
  }

  def visitTrue(index: Int) = {
    flushBuffer()
    out.append("true")
  }

  def visitNum(s: CharSequence, decIndex: Int, expIndex: Int, index: Int) = {
    flushBuffer()
    out.append(s)
  }

  def visitString(s: CharSequence, index: Int) = {
    flushBuffer()
    if (s == null) out.append("null")
    else Renderer.escape(out, s, true)
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
          if (c < ' ' || (c > '~' && unicode)) sb.append("\\u%04x" format c.toInt)
          else sb.append(c)
      }
      i += 1
    }
    sb.append('"')
  }
}