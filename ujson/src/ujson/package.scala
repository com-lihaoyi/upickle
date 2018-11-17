import upickle.core.NoOpVisitor

package object ujson{
  def transform[T](t: Transformable, v: upickle.core.Visitor[_, T]) = t.transform(v)

  def read(s: Transformable): Value.Value = transform(s, Value)

  def copy(t: Value.Value): Value.Value = transform(t, Value)

  def write(t: Value.Value, indent: Int = -1, escapeUnicode: Boolean = false): String = {
    transform(t, StringRenderer(indent, escapeUnicode)).toString
  }

  def writeTo(t: Value.Value, out: java.io.Writer, indent: Int = -1, escapeUnicode: Boolean = false): Unit = {
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
  @deprecated("use ujson.Value")
  type Js = Value
  @deprecated("use ujson.Value")
  val Js = Value
}