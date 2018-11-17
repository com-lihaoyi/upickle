package upickle
import upickle.api.MacroImplicits
import upickle.core.Visitor

trait JsReadWriters extends upickle.core.Types with MacroImplicits{

  implicit object JsValueR extends Reader[ujson.Value]{
    override def visitObject(length: Int, index: Int) = ujson.Value.visitObject(-1, index).narrow
    override def visitArray(length: Int, index: Int) = ujson.Value.visitArray(-1, index).narrow
    override def visitString(s: CharSequence, index: Int) = ujson.Value.visitString(s, index)
    override def visitFloat64StringParts(s: CharSequence, decIndex: Int, expIndex: Int, index: Int) = {
      ujson.Value.visitFloat64StringParts(s, decIndex, expIndex, index)
    }
    override def visitFloat64(d: Double, index: Int) = {
      ujson.Value.visitFloat64(d, index)
    }
    override def visitTrue(index: Int) = ujson.Value.visitTrue(index)
    override def visitFalse(index: Int) = ujson.Value.visitFalse(index)
    override def visitNull(index: Int) = ujson.Value.visitNull(index)
  }

  implicit def JsObjR: Reader[ujson.Obj] = JsValueR.narrow[ujson.Obj]
  implicit def JsArrR: Reader[ujson.Arr] = JsValueR.narrow[ujson.Arr]
  implicit def JsStrR: Reader[ujson.Str] = JsValueR.narrow[ujson.Str]
  implicit def JsNumR: Reader[ujson.Num] = JsValueR.narrow[ujson.Num]
  implicit def JsBoolR: Reader[ujson.Bool] = JsValueR.narrow[ujson.Bool]
  implicit def JsTrueR: Reader[ujson.True.type] = JsValueR.narrow[ujson.True.type]
  implicit def JsFalseR: Reader[ujson.False.type] = JsValueR.narrow[ujson.False.type]
  implicit def JsNullR: Reader[ujson.Null.type] = JsValueR.narrow[ujson.Null.type]


  implicit def JsObjW: Writer[ujson.Obj] = JsValueW.narrow[ujson.Obj]
  implicit def JsArrW: Writer[ujson.Arr] = JsValueW.narrow[ujson.Arr]
  implicit def JsStrW: Writer[ujson.Str] = JsValueW.narrow[ujson.Str]
  implicit def JsNumW: Writer[ujson.Num] = JsValueW.narrow[ujson.Num]
  implicit def JsBoolW: Writer[ujson.Bool] = JsValueW.narrow[ujson.Bool]
  implicit def JsTrueW: Writer[ujson.True.type] = JsValueW.narrow[ujson.True.type]
  implicit def JsFalseW: Writer[ujson.False.type] = JsValueW.narrow[ujson.False.type]
  implicit def JsNullW: Writer[ujson.Null.type] = JsValueW.narrow[ujson.Null.type]
  implicit object JsValueW extends Writer[ujson.Value] {
    def write0[R](out: Visitor[_, R], v: ujson.Value): R = {
      ujson.transform(v, out)
    }
  }
}