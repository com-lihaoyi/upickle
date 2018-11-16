package ujson.circe

import upickle.core.Visitor
import io.circe.{Json, JsonNumber}

import scala.collection.mutable.ArrayBuffer
object CirceJson extends ujson.AstTransformer[Json]{

  override def transform[T](j: Json, f: Visitor[_, T]) = j.fold(
    f.visitNull(-1),
    if (_) f.visitTrue(-1) else f.visitFalse(-1),
    n => f.visitNumRaw(n.toDouble, -1),
    f.visitString(_, -1),
    arr => transformArray(f, arr),
    obj => transformObject(f, obj.toList)
  )

  def visitArray(length: Int, index: Int) = new AstArrVisitor[Vector](x => Json.arr(x:_*))

  def visitObject(length: Int, index: Int) = new AstObjVisitor[ArrayBuffer[(String, Json)]](vs => Json.obj(vs:_*))

  def visitNull(index: Int) = Json.Null

  def visitFalse(index: Int) = Json.False

  def visitTrue(index: Int) = Json.True

  def visitNum(s: CharSequence, decIndex: Int, expIndex: Int, index: Int) = {
    Json.fromJsonNumber(
      if (decIndex == -1 && expIndex == -1) JsonNumber.fromIntegralStringUnsafe(s.toString)
      else JsonNumber.fromDecimalStringUnsafe(s.toString)
    )
  }

  def visitString(s: CharSequence, index: Int) = Json.fromString(s.toString)
}