package ujson

import scala.collection.generic.CanBuildFrom

trait AstTransformer[I] extends Transformer[I] with ujson.Visitor[I, I] {
  def transformArray[T](f: Visitor[_, T], items: TraversableOnce[I]) = {
    val ctx = f.visitArray(-1).narrow
    for(item <- items) ctx.visitValue(transform(item, ctx.subVisitor), -1)
    ctx.visitEnd(-1)
  }
  def transformObject[T](f: Visitor[_, T], items: TraversableOnce[(String, I)]) = {
    val ctx = f.visitObject(-1).narrow
    for (kv <- items) {
      ctx.visitKey(kv._1, -1)
      ctx.visitValue(transform(kv._2, ctx.subVisitor), -1)
    }
    ctx.visitEnd(-1)
  }

  class AstObjVisitor[T](build: T => I)
                        (implicit cbf: CanBuildFrom[Nothing, (String, I), T]) extends ObjVisitor[I, I] {

    private[this] var key: String = null
    private[this] val vs = cbf.apply()
    def subVisitor = AstTransformer.this
    def visitKey(s: CharSequence, index: Int): Unit = key = s.toString

    def visitValue(v: I, index: Int): Unit = vs += (key -> v)

    def visitEnd(index: Int) = build(vs.result)
  }
  class AstArrVisitor[T[_]](build: T[I] => I)
                           (implicit cbf: CanBuildFrom[Nothing, I, T[I]]) extends ArrVisitor[I, I] {
    def subVisitor = AstTransformer.this
    private[this] val vs = cbf.apply()
    def visitValue(v: I, index: Int): Unit = vs += v

    def visitEnd(index: Int) = build(vs.result())
  }
}
