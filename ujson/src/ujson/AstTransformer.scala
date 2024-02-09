package ujson
import upickle.core._

import upickle.core.compat._

import scala.language.higherKinds

trait AstTransformer[I] extends ujson.Transformer[I] with JsVisitor[I, I]{
  def apply(t: Readable): I = t.transform(this)

  def transformArray[T](f: Visitor[_, T], items: Iterable[I]) = {
    val ctx = f.visitArray(items.size, -1).narrow
    for(item <- items) ctx.visitValue(transform(item, ctx.subVisitor), -1)
    ctx.visitEnd(-1)
  }
  def transformObject[T](f: Visitor[_, T], items: Iterable[(String, I)]) = {
    val ctx = f.visitObject(items.size, true, -1).narrow
    for(kv <- items) {
      val keyVisitor = ctx.visitKey(-1)
      ctx.visitKeyValue(keyVisitor.visitString(kv._1, -1))
      ctx.visitValue(transform(kv._2, ctx.subVisitor), -1)
    }
    ctx.visitEnd(-1)
  }

  class AstObjVisitor[T](build: T => I)
                        (implicit factory: Factory[(String, I), T])extends ObjVisitor[I, I] {

    private[this] var key: String = null
    private[this] val vs = factory.newBuilder
    def subVisitor = AstTransformer.this
    def visitKey(index: Int) = upickle.core.StringVisitor
    def visitKeyValue(s: Any): Unit = key = s.toString

    def visitValue(v: I, index: Int): Unit = vs += (key -> v)

    def visitEnd(index: Int) = build(vs.result())
  }
  class AstArrVisitor[T[_]](build: T[I] => I)
                           (implicit factory: Factory[I, T[I]]) extends ArrVisitor[I, I]{
    def subVisitor = AstTransformer.this
    private[this] val vs = factory.newBuilder
    def visitValue(v: I, index: Int): Unit = vs += v

    def visitEnd(index: Int) = build(vs.result())
  }

}