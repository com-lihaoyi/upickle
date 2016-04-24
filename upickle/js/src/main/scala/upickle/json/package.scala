package upickle

import scalajs.js


package object json {

  def readJs(value: Any): Js.Value = value match{
    case s: String => Js.Str(s)
    case n: Double => Js.Num(n)
    case true => Js.True
    case false => Js.False
    case null => Js.Null
    case s: js.Array[_] => Js.Arr(s.map(readJs(_: Any)):_*)
    case s: js.Object => Js.Obj(s.asInstanceOf[js.Dictionary[_]].mapValues(readJs).toSeq:_*)
  }

  def read(s: String): Js.Value = {
    val parsed = try {
      js.JSON.parse(s)
    }catch{ case js.JavaScriptException(e: js.SyntaxError) =>
      throw Invalid.Json(e.message, s)
    }
    readJs(parsed)
  }

  def writeJs(value: Js.Value): Any = value match{
    case Js.Str(s) => s
    case Js.Num(n) => n
    case Js.True => true
    case Js.False => false
    case Js.Null => null
    case Js.Arr(children@_*) => js.Array(children.map(writeJs(_)):_*)
    case Js.Obj(kvs@_*) => js.Dictionary(kvs.map{case (k, v) => (k, writeJs(v))}:_*)
  }

  def write(v: Js.Value, indent: Int = 0): String = {
    js.JSON.stringify(
      writeJs(v).asInstanceOf[js.Any],
      null.asInstanceOf[js.Function2[String, js.Any, js.Any]],
      indent
    )
  }
}
