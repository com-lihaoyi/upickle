package upickle
import acyclic.file
import scalajs.js


package object json {

  def read(s: String): Js.Value = {
    def walk(value: Any): Js.Value = value match{
      case s: js.String => Js.Str(s)
      case n: js.Number => Js.Num(n)
      case true => Js.True
      case false => Js.False
      case null => Js.Null
      case s: js.Array[_] => Js.Arr(s.map(walk(_: Any)):_*)
      case s: js.Object => Js.Obj(s.asInstanceOf[js.Dictionary[_]].mapValues(walk).toSeq:_*)
    }
    val parsed = try {
      js.JSON.parse(s)
    }catch{ case js.JavaScriptException(e: js.SyntaxError) =>
      throw Invalid.Json(e.message, s)
    }
    walk(parsed)
  }
  def write(v: Js.Value): String = {
    def walk(value: Js.Value): Any = value match{
      case Js.Str(s) => s
      case Js.Num(n) => n
      case Js.True => true
      case Js.False => false
      case Js.Null => null
      case Js.Arr(children@_*) => js.Array(children.map(walk(_)):_*)
      case Js.Obj(kvs@_*) => js.Dictionary(kvs.map{case (k, v) => (k, walk(v))}:_*)
    }
    js.JSON.stringify(walk(v).asInstanceOf[js.Any])
  }
}
