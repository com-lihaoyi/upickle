package ujson.argonaut

import argonaut.{Json, JsonNumber, JsonObject}
import ujson.{ArrVisitor, ObjVisitor, Visitor}

import scala.collection.mutable

object ArgonautJson extends ujson.Transformer[Json]{
  override def transform[T](j: Json, f: Visitor[_, T]) = j.fold(
    f.visitNull(),
    if (_) f.visitTrue() else f.visitFalse(),
    n => n.toDouble match{
      case Some(d) => f.visitNumRaw(d, -1)
      case None =>
        val s = n.asJson.toString()
        f.visitNum(s, s.indexOf('.'), s.indexOf('E'), -1)
    },
    f.visitString(_),
    arr => {
      val ctx = f.visitArray().narrow
      for(x <- arr) ctx.visitValue(x, -1)
      ctx.visitEnd(-1)
    },
    obj => {
      val ctx = f.visitObject().narrow
      for(k <- obj.fields) {
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

      def visitEnd(index: Int) = Json.jArray(vs.toList)
    }

    def visitObject(index: Int) = new ObjVisitor[Json, Json] {
      var key: String = null

      val vs = mutable.ArrayBuffer.empty[(String, Json)]
      def visitKey(s: CharSequence, index: Int): Unit = key = s.toString

      def subVisitor = Builder

      def visitValue(v: Json, index: Int): Unit = vs.append(key.toString -> v)

      def visitEnd(index: Int) = Json.jObject(JsonObject.fromTraversableOnce(vs))
    }

    def visitNull(index: Int) = Json.jNull

    def visitFalse(index: Int) = Json.jFalse

    def visitTrue(index: Int) = Json.jTrue

    def visitNum(s: CharSequence, decIndex: Int, expIndex: Int, index: Int) = {
      Json.jNumber(JsonNumber.unsafeDecimal(s.toString))
    }

    def visitString(s: CharSequence, index: Int) = Json.jString(s.toString)
  }
}