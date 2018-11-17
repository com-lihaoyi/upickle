package ujson.json4s

import org.json4s.JsonAST._
import upickle.core.{ArrVisitor, ObjVisitor, Visitor}

object Json4sJson extends Json4sJson(false, false)

class Json4sJson(useBigDecimalForDouble: Boolean, useBigIntForLong: Boolean)
  extends ujson.AstTransformer[JValue] {
  def transform[T](j: JValue, f: Visitor[_, T]) = j match{
    case JArray(xs) => transformArray(f, xs)
    case JBool(b) => if (b) f.visitTrue(-1) else f.visitFalse(-1)
    case JDecimal(d) => f.visitFloat64String(d.toString, -1)
    case JDouble(d) => f.visitFloat64(d, -1)
    case JInt(i) => f.visitFloat64StringParts(i.toString, -1, -1, -1)
    case JLong(l) => f.visitFloat64StringParts(l.toString, -1, -1, -1)
    case JNothing => f.visitNull(-1)
    case JNull => f.visitNull(-1)
    case JObject(kvs) => transformObject(f, kvs)
    case JSet(xs) => transformArray(f, xs)
    case JString(s) => f.visitString(s, -1)
  }


  def visitArray(length: Int, index: Int) = new AstArrVisitor[List](x => JArray(x))
  def visitObject(length: Int, index: Int) = new AstObjVisitor[List[(String, JValue)]](JObject(_))

  def visitNull(index: Int) = JNull

  def visitFalse(index: Int) = JBool(false)

  def visitTrue(index: Int) = JBool(true)

  override def visitFloat64(d: Double, index: Int) = JDouble(d)

  def visitFloat64StringParts(s: CharSequence, decIndex: Int, expIndex: Int, index: Int) = {
    if (decIndex == -1 && expIndex == -1) {
      if (useBigIntForLong) JInt(BigInt(s.toString))
      else JLong(upickle.core.Util.parseLong(s, 0, s.length))
    } else {
      if (useBigDecimalForDouble) JDecimal(BigDecimal(s.toString))
      else JDouble(s.toString.toDouble)
    }
  }

  def visitString(s: CharSequence, index: Int) = JString(s.toString)
}