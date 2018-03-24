package ujson.circe

import ujson.{ArrVisitor, ObjVisitor, Visitor}

import scala.collection.mutable
import io.circe.{Json, JsonNumber, JsonObject}
object CirceJson extends ujson.Transformer[Json]{

  override def transform[T](j: Json, f: Visitor[_, T]) = j.fold(
    f.visitNull(),
    if (_) f.visitTrue() else f.visitFalse(),
    n => f.visitNumRaw(n.toDouble, -1),
    f.visitString(_),
    arr => {
      val ctx = f.visitArray().narrow
      for(x <- arr) ctx.visitValue(x, -1)
      ctx.visitEnd(-1)
    },
    obj => {
      val ctx = f.visitObject().narrow
      for(k <- obj.keys) {
        ctx.visitKey(k, -1)
        ctx.visitValue(obj.apply(k), -1)
      }
      ctx.visitEnd(-1)
    }
  )

  object Builder extends ujson.Visitor[Json, Json]{
    def visitArray(index: Int) = new ArrVisitor.Simple[Json, Json](Builder, x => Json.arr(x:_*))

    def visitObject(index: Int) = new ObjVisitor.Simple[Json, Json](Builder, vs => Json.obj(vs:_*))

    def visitNull(index: Int) = Json.Null

    def visitFalse(index: Int) = Json.False

    def visitTrue(index: Int) = Json.True

    def visitNum(s: CharSequence, decIndex: Int, expIndex: Int, index: Int) = {
      Json.fromJsonNumber(JsonNumber.fromString(s.toString).get)
    }

    def visitString(s: CharSequence, index: Int) = Json.fromString(s.toString)
  }
}