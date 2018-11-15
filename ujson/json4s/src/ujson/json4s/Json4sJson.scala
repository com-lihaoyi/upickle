package ujson.json4s

import org.json4s.JsonAST._
import ujson.{ArrVisitor, ObjVisitor, Visitor}

object Json4sJson extends Json4sJson(false, false)

class Json4sJson(useBigDecimalForDouble: Boolean, useBigIntForLong: Boolean)
  extends ujson.AstTransformer[JValue] {
  def transform[T](j: JValue, f: Visitor[_, T]) = j match{
    case JArray(xs) => transformArray(f, xs)
    case JBool(b) => if (b) f.visitTrue() else f.visitFalse()
    case JDecimal(d) => f.visitNumRawString(d.toString, -1)
    case JDouble(d) => f.visitNumRaw(d, -1)
    case JInt(i) => f.visitNum(i.toString, -1, -1, -1)
    case JLong(l) => f.visitNum(l.toString, -1, -1, -1)
    case JNothing => f.visitNull()
    case JNull => f.visitNull()
    case JObject(kvs) => transformObject(f, kvs)
    case JSet(xs) => transformArray(f, xs)
    case JString(s) => f.visitString(s)
  }


  def visitArray(length: Int, index: Int) = new AstArrVisitor[List](x => JArray(x))
  def visitObject(length: Int, index: Int) = new AstObjVisitor[List[(String, JValue)]](JObject(_))

  def visitNull(index: Int) = JNull

  def visitFalse(index: Int) = JBool(false)

  def visitTrue(index: Int) = JBool(true)

  override def visitNumRaw(d: Double, index: Int) = JDouble(d)

  def visitNum(s: CharSequence, decIndex: Int, expIndex: Int, index: Int) = {
    if (decIndex == -1 && expIndex == -1) {
      if (useBigIntForLong) JInt(BigInt(s.toString))
      else JLong(ujson.util.Util.parseLong(s, 0, s.length))
    } else {
      if (useBigDecimalForDouble) JDecimal(BigDecimal(s.toString))
      else JDouble(s.toString.toDouble)
    }
  }

  def visitString(s: CharSequence, index: Int) = JString(s.toString)
}