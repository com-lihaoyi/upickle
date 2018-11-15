package ujson.play


import play.api.libs.json._
import ujson.Visitor

import scala.collection.mutable.ArrayBuffer

object PlayJson extends ujson.AstTransformer[JsValue] {
  def transform[T](j: JsValue, f: Visitor[_, T]): T = j match{
    case JsArray(xs) => transformArray(f, xs)
    case JsBoolean(b) => if (b) f.visitTrue() else f.visitFalse()
    case JsNull => f.visitNull()
    case JsNumber(d) => f.visitNumRawString(d.toString, -1)
    case JsObject(kvs) => transformObject(f, kvs)
    case JsString(s) => f.visitString(s)
  }
  def visitArray(length: Int, index: Int) = new AstArrVisitor[Array](JsArray(_))

  def visitObject(length: Int, index: Int) = new AstObjVisitor[ArrayBuffer[(String, JsValue)]](JsObject(_))

  def visitNull(index: Int) = JsNull

  def visitFalse(index: Int) = JsBoolean(false)

  def visitTrue(index: Int) = JsBoolean(true)

  def visitNum(s: CharSequence, decIndex: Int, expIndex: Int, index: Int) = {
    JsNumber(BigDecimal(s.toString))
  }

  def visitString(s: CharSequence, index: Int) = JsString(s.toString)
}