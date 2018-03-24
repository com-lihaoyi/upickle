package ujson

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

trait AstTransformer[I] extends Transformer[I] {
  val Builder: ujson.Visitor[I, I]
  type Builder = ujson.Visitor[I, I]
  def transformArray[T](f: Visitor[_, T], items: TraversableOnce[I]) = {
    val ctx = f.visitArray(-1).narrow
    for(item <- items) ctx.visitValue(transform(item, ctx.subVisitor), -1)
    ctx.visitEnd(-1)
  }
  def transformObject[T](f: Visitor[_, T], items: TraversableOnce[(String, I)]) = {
    val ctx = f.visitObject(-1).narrow
    for((k, item) <- items) {
      ctx.visitKey(k, -1)
      ctx.visitValue(transform(item, ctx.subVisitor), -1)
    }
    ctx.visitEnd(-1)
  }

  class AstObjVisitor(build: ArrayBuffer[(String, I)] => I) extends ObjVisitor[I, I] {

    private[this] var key: String = null
    private[this] val vs = mutable.ArrayBuffer.empty[(String, I)]
    def subVisitor = Builder
    def visitKey(s: CharSequence, index: Int): Unit = key = s.toString

    def visitValue(v: I, index: Int): Unit = vs += (key -> v)

    def visitEnd(index: Int) = build(vs)
  }
  class AstArrVisitor(build: ArrayBuffer[I] => I) extends ArrVisitor[I, I]{
    def subVisitor = Builder
    private[this] val vs = mutable.ArrayBuffer.empty[I]
    def visitValue(v: I, index: Int): Unit = vs.append(v)

    def visitEnd(index: Int) = build(vs)
  }
}
