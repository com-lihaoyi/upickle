package ujson.play


import play.api.libs.json._
import ujson.Visitor

object PlayJson extends ujson.AstTransformer[JsValue] {
  def transform[T](j: JsValue, f: Visitor[_, T]): T = j match{
    case JsArray(xs) => transformArray(f, xs)
    case JsBoolean(b) => if (b) f.visitTrue() else f.visitFalse()
    case JsNull => f.visitNull()
    case JsNumber(d) =>
      val s = d.toString()
      f.visitNum(s, s.indexOf('.'), s.indexOf('E'), -1)
    case JsObject(kvs) => transformObject(f, kvs)
    case JsString(s) => f.visitString(s)
  }
  object Builder extends Builder{
    def visitArray(index: Int) = new AstArrVisitor(JsArray(_))

    def visitObject(index: Int) = new AstObjVisitor(JsObject(_))

    def visitNull(index: Int) = JsNull

    def visitFalse(index: Int) = JsBoolean(false)

    def visitTrue(index: Int) = JsBoolean(true)

    def visitNum(s: CharSequence, decIndex: Int, expIndex: Int, index: Int) = {
      JsNumber(BigDecimal(s.toString))
    }

    def visitString(s: CharSequence, index: Int) = JsString(s.toString)
  }
}