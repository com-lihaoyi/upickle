package object ujson{
  def transform[T](t: Transformable, v: Visitor[_, T]) = t.transform(v)

  def read(s: Transformable): Js.Value = transform(s, Js)

  def copy(t: Js.Value): Js.Value = transform(t, Js)

  def write(t: Js.Value, indent: Int = -1): String = {
    transform(t, StringRenderer(indent)).toString
  }

  def writeTo(t: Js.Value, out: java.io.Writer, indent: Int = -1): Unit = {
    transform(t, Renderer(out, indent))
  }

  def validate(s: Transformable): Unit = transform(s, NoOpVisitor)

  def reformat(s: Transformable, indent: Int = -1): String = {
    transform(s, StringRenderer(indent)).toString
  }

  def reformatTo(s: Transformable, out: java.io.Writer, indent: Int = -1): Unit = {
    transform(s, Renderer(out, indent)).toString
  }
  // End ujson
}