package upickle

import upickle.core.Visitor
import upickle.implicits.MacroImplicits


trait JsReadWriters extends upickle.core.Types with MacroImplicits {

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

  // This trait has no instance. Its only purpose is to deprioritize implicit
  // lookup of JsValueR and JsValueW over all other instances. This is not
  // required in Scala 2, but is required in Dotty to avoid implicit ambiguities
  // when summoning readers and writers in certain circumstances.
  trait ImplicitDummy

  implicit def JsValueR(implicit dummy: ImplicitDummy = null): Reader[ujson.Value] = new Reader.Delegate(ujson.Value)
  implicit def JsValueW(implicit dummy: ImplicitDummy = null): Writer[ujson.Value] = new Writer[ujson.Value] {
    def write0[R](out: Visitor[_, R], v: ujson.Value): R = ujson.transform(v, out)
  }
}
