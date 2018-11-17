import ujson.{Js, StringRenderer, Transformable, transform}

package object upickle {
  @deprecated("use ujson.Value")
  val Js = ujson.Js
  object json {
    @deprecated("use ujson.read")
    def read(s: Transformable): Js.Value = transform(s, Js)

    @deprecated("use ujson.write")
    def write(t: Js.Value, indent: Int = -1, escapeUnicode: Boolean = false): String = {
      transform(t, StringRenderer(indent, escapeUnicode)).toString
    }
  }
}
