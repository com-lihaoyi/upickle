package upickle


import java.io.{ByteArrayOutputStream, StringWriter, Writer}

import jawn.RawFContext

import scala.annotation.switch

class Renderer(out: java.io.Writer,
               var indent: Int = -1,
               var depth: Int = 0) extends jawn.Facade[Unit]{
  def singleContext() = ???

  def arrayContext() = new RawFContext[Unit, Unit] {
    out.append("[")
    var first = true
    depth += 1
    def facade = Renderer.this
    def visitKey(s: CharSequence, index: Int): Unit = ???
    def add(v: Unit, index: Int): Unit = {
      if (first) first = false
      else out.append(", ")
      renderIndent()
    }
    def finish(index: Int): Unit = {
      depth -= 1
      renderIndent()
      out.append("]")
    }
    def isObj = false
  }

  def objectContext() = new RawFContext[Unit, Unit] {
    out.append("{")
    var first = true
    depth += 1
    def facade = Renderer.this
    def visitKey(s: CharSequence, index: Int): Unit = {
      if (first) first = false
      else out.append(", ")
      renderIndent()
      Renderer.escape(out, s, true)
      out.append(": ")
    }
    def add(v: Unit, index: Int): Unit = {}
    def finish(index: Int): Unit = {
      depth -= 1
      renderIndent()
      out.append("}")
    }
    def isObj = false
  }

  def jnull() = out.append("null")

  def jfalse() = out.append("false")

  def jtrue() = out.append("true")

  def jnum(s: CharSequence, decIndex: Int, expIndex: Int) = out.append(s)

  def jstring(s: CharSequence) =
    if (s == null) out.append("null")
    else Renderer.escape(out, s, true)

  final def renderIndent() = {
    if (indent == -1) ()
    else {
      out.append('\n')
      for(_ <- 0 until (indent * depth)) out.append(' ')
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
//trait Renderer {
//  final def render(sb: Writer, depth: Int, jv: Js.Value, indent: Int): Unit =
//    jv match {
//      case Js.Null => sb.append("null")
//      case Js.True => sb.append("true")
//      case Js.False => sb.append("false")
//      case Js.Num(n) => sb.append(if (n == n.toLong) n.toLong.toString else n.toString)
//      case Js.Str(s) => renderString(sb, s)
//      case Js.Arr(vs@_*) => renderArray(sb, depth, vs, indent)
//      case Js.Obj(vs@_*) => renderObject(sb, depth, canonicalizeObject(vs), indent)
//    }
//
//  def canonicalizeObject(vs: Seq[(CharSequence, Js.Value)]): Iterator[(CharSequence, Js.Value)]
//
//  def renderString(sb: Writer, s: CharSequence): Unit
//
//  final def renderIndent(sb: Writer, depth: Int, indent: Int) = {
//    if (indent == 0) ()
//    else {
//      sb.append('\n')
//      for(_ <- 0 until (indent * depth)) sb.append(' ')
//    }
//  }
//  final def renderArray(sb: Writer, depth: Int, vs: Seq[Js.Value], indent: Int): Unit = {
//    if (vs.isEmpty) sb.append("[]")
//    else {
//      sb.append('[')
//      renderIndent(sb, depth + 1, indent)
//      render(sb, depth + 1, vs(0), indent)
//      var i = 1
//      while (i < vs.length) {
//        sb.append(',')
//        renderIndent(sb, depth + 1, indent)
//        render(sb, depth + 1, vs(i), indent)
//        i += 1
//      }
//      renderIndent(sb, depth, indent)
//      sb.append(']')
//    }
//  }
//
//  final def renderObject(sb: Writer, depth: Int, it: Iterator[(CharSequence, Js.Value)], indent: Int): Unit = {
//    if (!it.hasNext) return { sb.append("{}"); () }
//    val (k0, v0) = it.next
//    sb.append('{')
//    renderIndent(sb, depth + 1, indent)
//    renderString(sb, k0)
//    sb.append(':')
//    if(indent != 0) sb.append(' ')
//    render(sb, depth + 1, v0, indent)
//    while (it.hasNext) {
//      val (k, v) = it.next
//      sb.append(',')
//      renderIndent(sb, depth + 1, indent)
//      renderString(sb, k)
//      sb.append(':')
//      if(indent != 0) sb.append(' ')
//      render(sb, depth + 1, v, indent)
//    }
//    renderIndent(sb, depth, indent)
//    sb.append('}')
//  }
//
//  final def escape(sb: Writer, s: CharSequence, unicode: Boolean): Unit = {
//    sb.append('"')
//    var i = 0
//    val len = s.length
//    while (i < len) {
//      (s.charAt(i): @switch) match {
//        case '"' => sb.append("\\\"")
//        case '\\' => sb.append("\\\\")
//        case '\b' => sb.append("\\b")
//        case '\f' => sb.append("\\f")
//        case '\n' => sb.append("\\n")
//        case '\r' => sb.append("\\r")
//        case '\t' => sb.append("\\t")
//        case c =>
//          if (c < ' ' || (c > '~' && unicode)) sb.append("\\u%04x" format c.toInt)
//          else sb.append(c)
//      }
//      i += 1
//    }
//    sb.append('"')
//  }
//}
//
//object FastRenderer extends Renderer {
//  def canonicalizeObject(vs: Seq[(CharSequence, Js.Value)]): Iterator[(CharSequence, Js.Value)] =
//    vs.iterator
//  def renderString(sb: Writer, s: CharSequence): Unit =
//    escape(sb, s, false)
//}
//object SortingRenderer extends Renderer {
//  def canonicalizeObject(vs: Seq[(CharSequence, Js.Value)]): Iterator[(CharSequence, Js.Value)] =
//    vs.map{case (k, v) => (k.toString, v)}.sortBy(_._1).iterator
//  def renderString(sb: Writer, s: CharSequence): Unit =
//    escape(sb, s, false)
//}
//trait JsonPackageWriters{
//  def write(v: Js.Value, indent: Int = 0, sortKeys: Boolean = false): String = {
//    val sb = new StringWriter()
//    writeTo(v, sb, indent, sortKeys)
//    sb.toString()
//  }
//  def writeTo(v: Js.Value, sb: Writer, indent: Int = 0, sortKeys: Boolean = false): Unit = {
//    val renderer =
//      if (!sortKeys) FastRenderer
//      else SortingRenderer
//    renderer.render(sb, 0, v, indent)
//  }
//}