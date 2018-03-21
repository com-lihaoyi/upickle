package upickle
package visitors

import java.io.{ByteArrayOutputStream, StringWriter, Writer}

import upickle.jawn.{ArrVisitor, ObjArrVisitor, ObjVisitor}

import scala.annotation.switch

class Renderer(out: java.io.Writer,
               var indent: Int = -1,
               var depth: Int = 0) extends upickle.jawn.Visitor[Unit, Unit]{
  val colonSnippet = if (indent == -1) ":" else ": "
  def singleContext(index: Int) = ???

  var commaBuffered = false

  def flushBuffer() = {
    if (commaBuffered) {
      commaBuffered = false
      out.append(',')
      renderIndent()
    }
  }
  def arrayContext(index: Int) = new ArrVisitor[Unit, Unit] {
    flushBuffer()
    out.append('[')

    depth += 1
    renderIndent()
    def subVisitor = Renderer.this
    def add(v: Unit, index: Int): Unit = {
      flushBuffer()
      commaBuffered = true
    }
    def finish(index: Int): Unit = {
      commaBuffered = false
      depth -= 1
      renderIndent()
      out.append(']')
    }
  }

  def objectContext(index: Int) = new ObjVisitor[Unit, Unit] {
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
    def add(v: Unit, index: Int): Unit = {
      commaBuffered = true
    }
    def finish(index: Int): Unit = {
      commaBuffered = false
      depth -= 1
      renderIndent()
      out.append('}')
    }
  }

  def jnull(index: Int) = {
    flushBuffer()
    out.append("null")
  }

  def jfalse(index: Int) = {
    flushBuffer()
    out.append("false")
  }

  def jtrue(index: Int) = {
    flushBuffer()
    out.append("true")
  }

  def jnum(s: CharSequence, decIndex: Int, expIndex: Int, index: Int) = {
    flushBuffer()
    out.append(s)
  }

  def jstring(s: CharSequence, index: Int) = {
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
object Renderer{
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