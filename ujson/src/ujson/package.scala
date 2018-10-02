package object ujson{
  def transform[T](t: Transformable, v: Visitor[_, T]) = t.transform(v)

  def read(s: Transformable): Js.Value = transform(s, Js)

  def copy(t: Js.Value): Js.Value = transform(t, Js)

  def write(t: Js.Value, indent: Int = -1, escapeUnicode: Boolean = false): String = {
    transform(t, StringRenderer(indent, escapeUnicode)).toString
  }

  def writeTo(t: Js.Value, out: java.io.Writer, indent: Int = -1, escapeUnicode: Boolean = false): Unit = {
    transform(t, Renderer(out, indent, escapeUnicode))
  }

  def validate(s: Transformable): Unit = transform(s, NoOpVisitor)

  def reformat(s: Transformable, indent: Int = -1, escapeUnicode: Boolean = false): String = {
    transform(s, StringRenderer(indent, escapeUnicode)).toString
  }

  def reformatTo(s: Transformable, out: java.io.Writer, indent: Int = -1, escapeUnicode: Boolean = false): Unit = {
    transform(s, Renderer(out, indent, escapeUnicode)).toString
  }
  // End ujson
}