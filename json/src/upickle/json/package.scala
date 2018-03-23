package upickle

package object json{
  def transform[T](t: Transformable, v: Visitor[_, T]) = t.transform(v)

  def read(s: Transformable): Js.Value = transform(s, Js.Builder)

  def write(t: Js.Value, indent: Int = -1): String = {
    transform(t, StringRenderer(indent)).toString
  }

  def writeTo(t: Js.Value, out: java.io.Writer, indent: Int = -1): String = {
    transform(t, Renderer(out, indent)).toString
  }

  def validate(s: Transformable): Unit = transform(s, NoOpVisitor)

  def reformat(s: Transformable, indent: Int = -1) = {
    transform(s, StringRenderer(indent)).toString
  }

  def reformatTo(s: Transformable, out: java.io.Writer, indent: Int = -1) = {
    transform(s, Renderer(out, indent)).toString
  }
}