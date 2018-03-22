package upickle

import upickle.json.{Transformable, Visitor}


package object json{
  def read(s: Transformable): Js.Value = s.transform(Js.Builder)
  def write(t: Js.Value): String = {
    Js.transform(t, StringRenderer()).toString
  }

  def transform[T](t: Transformable, v: Visitor[_, T]) = t.transform(v)
}