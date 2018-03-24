package ujson.json4s


import scala.collection.mutable
import org.json4s.JsonAST._
import ujson.{ArrVisitor, ObjVisitor, Visitor}

object Json4sJson extends Json4sJson(false, false)

class Json4sJson(useBigDecimalForDouble: Boolean, useBigIntForLong: Boolean)
  extends ujson.Transformer[JValue] {
  def transform[T](j: JValue, f: Visitor[_, T]) = j match{
    case JArray(xs) =>
      val ctx = f.visitArray().narrow
      for(x <- xs) ctx.visitValue(x, -1)
      ctx.visitEnd(-1)
    case JBool(b) => if (b) f.visitTrue() else f.visitFalse()
    case JDecimal(d) =>
      val s = d.toString
      f.visitNum(s, s.indexOf('.'), s.indexOf('E'), -1)
    case JDouble(d) => f.visitNumRaw(d, -1)
    case JInt(i) => f.visitNum(i.toString, -1, -1, -1)
    case JLong(l) => f.visitNum(l.toString, -1, -1, -1)
    case JNothing => f.visitNull()
    case JNull => f.visitNull()
    case JObject(kvs) =>
      val ctx = f.visitObject().narrow
      for((k, v) <- kvs) {
        ctx.visitKey(k, -1)
        ctx.visitValue(v, -1)
      }
      ctx.visitEnd(-1)
    case JSet(xs) =>
      val ctx = f.visitArray().narrow
      for(x <- xs) ctx.visitValue(x, -1)
      ctx.visitEnd(-1)
    case JString(s) => f.visitString(s)
  }


  object Builder extends ujson.Visitor[JValue, JValue] {
    def visitArray(index: Int) = new ArrVisitor.Simple[JValue, JValue](
      Builder,
      x => JArray(x.toList)

)
    def visitObject(index: Int) = new ObjVisitor.Simple[JValue, JValue](
      Builder,
      x => JObject(x.toList)
    )

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
}