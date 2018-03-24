package ujson.play


import play.api.libs.json._
import ujson.{ArrVisitor, ObjVisitor, Visitor}

import scala.collection.mutable

object PlayJson extends ujson.Transformer[JsValue] {
  def transform[T](j: JsValue, f: Visitor[_, T]): T = j match{
    case JsArray(xs) =>
      val ctx = f.visitArray().narrow
      for(x <- xs) ctx.visitValue(x, -1)
      ctx.visitEnd(-1)
    case JsBoolean(b) => if (b) f.visitTrue() else f.visitFalse()
    case JsNull => f.visitNull()
    case JsNumber(d) =>
      val s = d.toString()
      f.visitNum(s, s.indexOf('.'), s.indexOf('E'), -1)
    case JsObject(kvs) =>
      val ctx = f.visitObject().narrow
      for((k, v) <- kvs) {
        ctx.visitKey(k, -1)
        ctx.visitValue(v, -1)
      }
      ctx.visitEnd(-1)
    case JsString(s) => f.visitString(s)
  }
  object Builder extends ujson.Visitor[JsValue, JsValue]{
    def visitArray(index: Int) = new ArrVisitor.Simple[JsValue, JsValue](Builder, JsArray(_))

    def visitObject(index: Int) = new ObjVisitor.Simple[JsValue, JsValue](Builder, JsObject(_))

    def visitNull(index: Int) = JsNull

    def visitFalse(index: Int) = JsBoolean(false)

    def visitTrue(index: Int) = JsBoolean(true)

    def visitNum(s: CharSequence, decIndex: Int, expIndex: Int, index: Int) = {
      JsNumber(BigDecimal(s.toString))
    }

    def visitString(s: CharSequence, index: Int) = JsString(s.toString)
  }

  
}