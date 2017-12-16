package upickle.json

import java.io.{ByteArrayOutputStream, StringWriter, Writer}

import upickle.Js

import scala.annotation.switch

trait Renderer {
  final def render(sb: Writer, depth: Int, jv: Js.Value, indent: Int): Unit =
    jv match {
      case Js.Null => sb.append("null")
      case Js.True => sb.append("true")
      case Js.False => sb.append("false")
      case Js.Num(n) => sb.append(if (n == n.toLong) n.toLong.toString else n.toString)
      case Js.Str(s) => renderString(sb, s)
      case Js.Arr(vs@_*) => renderArray(sb, depth, vs, indent)
      case Js.Obj(vs@_*) => renderObject(sb, depth, canonicalizeObject(vs), indent)
    }

  def canonicalizeObject(vs: Seq[(CharSequence, Js.Value)]): Iterator[(CharSequence, Js.Value)]

  def renderString(sb: Writer, s: CharSequence): Unit

  final def renderIndent(sb: Writer, depth: Int, indent: Int) = {
    if (indent == 0) ()
    else {
      sb.append('\n')
      for(_ <- 0 until (indent * depth)) sb.append(' ')
    }
  }
  final def renderArray(sb: Writer, depth: Int, vs: Seq[Js.Value], indent: Int): Unit = {
    if (vs.isEmpty) sb.append("[]")
    else {
      sb.append('[')
      renderIndent(sb, depth + 1, indent)
      render(sb, depth + 1, vs(0), indent)
      var i = 1
      while (i < vs.length) {
        sb.append(',')
        renderIndent(sb, depth + 1, indent)
        render(sb, depth + 1, vs(i), indent)
        i += 1
      }
      renderIndent(sb, depth, indent)
      sb.append(']')
    }
  }

  final def renderObject(sb: Writer, depth: Int, it: Iterator[(CharSequence, Js.Value)], indent: Int): Unit = {
    if (!it.hasNext) return { sb.append("{}"); () }
    val (k0, v0) = it.next
    sb.append('{')
    renderIndent(sb, depth + 1, indent)
    renderString(sb, k0)
    sb.append(':')
    if(indent != 0) sb.append(' ')
    render(sb, depth + 1, v0, indent)
    while (it.hasNext) {
      val (k, v) = it.next
      sb.append(',')
      renderIndent(sb, depth + 1, indent)
      renderString(sb, k)
      sb.append(':')
      if(indent != 0) sb.append(' ')
      render(sb, depth + 1, v, indent)
    }
    renderIndent(sb, depth, indent)
    sb.append('}')
  }

  final def escape(sb: Writer, s: CharSequence, unicode: Boolean): Unit = {
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

object FastRenderer extends Renderer {
  def canonicalizeObject(vs: Seq[(CharSequence, Js.Value)]): Iterator[(CharSequence, Js.Value)] =
    vs.iterator
  def renderString(sb: Writer, s: CharSequence): Unit =
    escape(sb, s, false)
}
object SortingRenderer extends Renderer {
  def canonicalizeObject(vs: Seq[(CharSequence, Js.Value)]): Iterator[(CharSequence, Js.Value)] =
    vs.map{case (k, v) => (k.toString, v)}.sortBy(_._1).iterator
  def renderString(sb: Writer, s: CharSequence): Unit =
    escape(sb, s, false)
}
trait JsonPackageWriters{
  def write(v: Js.Value, indent: Int = 0, sortKeys: Boolean = false): String = {
    val sb = new StringWriter()
    writeTo(v, sb, indent, sortKeys)
    sb.toString()
  }
  def writeTo(v: Js.Value, sb: Writer, indent: Int = 0, sortKeys: Boolean = false): Unit = {
    val renderer =
      if (!sortKeys) FastRenderer
      else SortingRenderer
    renderer.render(sb, 0, v, indent)
  }
}