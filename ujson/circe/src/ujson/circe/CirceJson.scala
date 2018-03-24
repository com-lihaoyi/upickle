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
    def visitArray(index: Int) = new ArrVisitor[Json, Json] {
      val vs = mutable.ArrayBuffer.empty[Json]
      def subVisitor = Builder

      def visitValue(v: Json, index: Int): Unit = vs.append(v)

      def visitEnd(index: Int) = Json.arr(vs:_*)
    }

    def visitObject(index: Int) = new ObjVisitor[Json, Json] {
      var key: String = null

      val vs = mutable.ArrayBuffer.empty[(String, Json)]
      def visitKey(s: CharSequence, index: Int): Unit = key = s.toString

      def subVisitor = Builder

      def visitValue(v: Json, index: Int): Unit = vs.append(key.toString -> v)

      def visitEnd(index: Int) = Json.obj(vs:_*)
    }

    def visitNull(index: Int) = Json.Null

    def visitFalse(index: Int) = Json.False

    def visitTrue(index: Int) = Json.True

    def visitNum(s: CharSequence, decIndex: Int, expIndex: Int, index: Int) = {
      Json.fromJsonNumber(JsonNumber.fromString(s.toString).get)
    }

    def visitString(s: CharSequence, index: Int) = Json.fromString(s.toString)
  }
}