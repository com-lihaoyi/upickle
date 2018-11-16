import upickle.core.NoOpVisitor

package object upack{
  def transform[T](t: Transformable, v: upickle.core.Visitor[_, T]) = t.transform(v)

  def read(s: Transformable): Msg = transform(s, Msg)

  def copy(t: Msg): Msg = transform(t, Msg)

  def write(t: Msg): Array[Byte] = {
    transform(t, new MsgPackWriter()).toByteArray
  }

  def writeTo(t: Msg, out: java.io.OutputStream): Unit = {
    transform(t, new MsgPackWriter(out))
  }

  def validate(s: Transformable): Unit = transform(s, NoOpVisitor)
}