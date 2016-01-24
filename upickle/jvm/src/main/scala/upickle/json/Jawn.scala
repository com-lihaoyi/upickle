package upickle
package json
import jawn._
import collection.mutable
import acyclic.file
import scala.annotation.switch

private[json] object JawnFacade extends MutableFacade[Js.Value] {
  def jnull() = Js.Null
  def jfalse() = Js.False
  def jtrue() = Js.True
  def jnum(s: String) = Js.Num(s.toDouble)
  def jint(s: String) = Js.Num(s.toDouble)
  def jstring(s: String) = Js.Str(s)
  def jarray(vs: mutable.ArrayBuffer[Js.Value]) = Js.Arr(vs:_*)
  def jobject(vs: mutable.ArrayBuffer[(String, Js.Value)]) = Js.Obj(vs:_*)
}

private[json] trait MutableFacade[J] extends Facade[J] {
  def jarray(vs: mutable.ArrayBuffer[J]): J
  def jobject(vs: mutable.ArrayBuffer[(String, J)]): J

  def singleContext() = new FContext[J] {
    var value: J = _
    def add(s: String) { value = jstring(s) }
    def add(v: J) { value = v }
    def finish: J = value
    def isObj: Boolean = false
  }

  def arrayContext() = new FContext[J] {
    val vs = mutable.ArrayBuffer.empty[J]
    def add(s: String) { vs.append(jstring(s)) }
    def add(v: J) { vs.append(v) }
    def finish: J = jarray(vs)
    def isObj: Boolean = false
  }

  def objectContext() = new FContext[J] {
    var key: String = null
    val vs = mutable.ArrayBuffer.empty[(String, J)]
    def add(s: String): Unit =
      if (key == null) { key = s } else { vs += (key -> jstring(s)); key = null }
    def add(v: J): Unit =
    { vs += (key -> v); key = null }
    def finish = jobject(vs)
    def isObj = true
  }
}
sealed trait Renderer {
  final def render(jv: Js.Value): String = {
    val sb = new StringBuilder
    render(sb, 0, jv, 0)
    sb.toString
  }

  final def render(sb: StringBuilder, depth: Int, jv: Js.Value, indent: Int): Unit =
    jv match {
      case Js.Null => sb.append("null")
      case Js.True => sb.append("true")
      case Js.False => sb.append("false")
      case Js.Num(n) => sb.append(if (n == n.toInt) n.toInt.toString else n.toString)
      case Js.Str(s) => renderString(sb, s)
      case Js.Arr(vs@_*) => renderArray(sb, depth, vs, indent)
      case Js.Obj(vs@_*) => renderObject(sb, depth, canonicalizeObject(vs), indent)
    }

  def canonicalizeObject(vs: Seq[(String, Js.Value)]): Iterator[(String, Js.Value)]

  def renderString(sb: StringBuilder, s: String): Unit

  final def renderIndent(sb: StringBuilder, depth: Int, indent: Int) = {
    if (indent == 0) ()
    else {
      sb.append('\n')
      for(_ <- 0 until (indent * depth)) sb.append(' ')
    }
  }
  final def renderArray(sb: StringBuilder, depth: Int, vs: Seq[Js.Value], indent: Int): Unit = {
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

  final def renderObject(sb: StringBuilder, depth: Int, it: Iterator[(String, Js.Value)], indent: Int): Unit = {
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

  final def escape(sb: StringBuilder, s: String, unicode: Boolean): Unit = {
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
  def canonicalizeObject(vs: Seq[(String, Js.Value)]): Iterator[(String, Js.Value)] =
    vs.iterator
  def renderString(sb: StringBuilder, s: String): Unit =
    escape(sb, s, false)
}