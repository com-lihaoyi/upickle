package ujson

import scala.collection.generic.CanBuildFrom
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

trait AstTransformer[I] extends Transformer[I] with ujson.Visitor[I, I]{
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
    def subVisitor = AstTransformer.this
    def visitKey(s: CharSequence, index: Int): Unit = key = s.toString

    def visitValue(v: I, index: Int): Unit = vs += (key -> v)

    def visitEnd(index: Int) = build(vs)
  }
  class AstArrVisitor[T[_]](build: T[I] => I)
                           (implicit cbf: CanBuildFrom[Nothing, I, T[I]])extends ArrVisitor[I, I]{
    def subVisitor = AstTransformer.this
    private[this] val vs = cbf.apply()
    def visitValue(v: I, index: Int): Unit = vs += v

    def visitEnd(index: Int) = build(vs.result())
  }
}
