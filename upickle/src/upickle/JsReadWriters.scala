package upickle
import upickle.Js
import upickle.api.MacroImplicits
import upickle.core.Visitor

trait JsReadWriters extends upickle.core.Types with MacroImplicits{

  implicit object JsValueR extends Reader[Js.Value]{
    override def visitObject(length: Int, index: Int) = Js.visitObject(-1, index).narrow
    override def visitArray(length: Int, index: Int) = Js.visitArray(-1, index).narrow
    override def visitString(s: CharSequence, index: Int) = Js.visitString(s, index)
    override def visitFloat64StringParts(s: CharSequence, decIndex: Int, expIndex: Int, index: Int) = {
      Js.visitFloat64StringParts(s, decIndex, expIndex, index)
    }
    override def visitFloat64(d: Double, index: Int) = {
      Js.visitFloat64(d, index)
    }
    override def visitTrue(index: Int) = Js.visitTrue(index)
    override def visitFalse(index: Int) = Js.visitFalse(index)
    override def visitNull(index: Int) = Js.visitNull(index)
  }

  implicit def JsObjR: Reader[Js.Obj] = JsValueR.narrow[Js.Obj]
  implicit def JsArrR: Reader[Js.Arr] = JsValueR.narrow[Js.Arr]
  implicit def JsStrR: Reader[Js.Str] = JsValueR.narrow[Js.Str]
  implicit def JsNumR: Reader[Js.Num] = JsValueR.narrow[Js.Num]
  implicit def JsBoolR: Reader[Js.Bool] = JsValueR.narrow[Js.Bool]
  implicit def JsTrueR: Reader[Js.True.type] = JsValueR.narrow[Js.True.type]
  implicit def JsFalseR: Reader[Js.False.type] = JsValueR.narrow[Js.False.type]
  implicit def JsNullR: Reader[Js.Null.type] = JsValueR.narrow[Js.Null.type]


  implicit def JsObjW: Writer[Js.Obj] = JsValueW.narrow[Js.Obj]
  implicit def JsArrW: Writer[Js.Arr] = JsValueW.narrow[Js.Arr]
  implicit def JsStrW: Writer[Js.Str] = JsValueW.narrow[Js.Str]
  implicit def JsNumW: Writer[Js.Num] = JsValueW.narrow[Js.Num]
  implicit def JsBoolW: Writer[Js.Bool] = JsValueW.narrow[Js.Bool]
  implicit def JsTrueW: Writer[Js.True.type] = JsValueW.narrow[Js.True.type]
  implicit def JsFalseW: Writer[Js.False.type] = JsValueW.narrow[Js.False.type]
  implicit def JsNullW: Writer[Js.Null.type] = JsValueW.narrow[Js.Null.type]
  implicit object JsValueW extends Writer[Js.Value] {
    def write0[R](out: Visitor[_, R], v: Js.Value): R = {
      Js.transform(v, out)
    }
  }
}