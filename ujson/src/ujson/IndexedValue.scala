package ujson

import ujson._
import upickle.core.Util.reject
import scala.collection.mutable
import upickle.core.{Visitor, ObjVisitor, ArrVisitor, Abort, AbortException}

/**
  * A version of [[ujson.Value]] that keeps the index positions of the various AST
  * nodes it is constructing. Usually not necessary, but sometimes useful if you
  * want to work with an AST but still provide source-index error positions if
  * something goes wrong
  */
sealed trait IndexedValue {
  def index: Int
}

object IndexedValue extends Transformer[IndexedValue]{
  
  case class Str(index: Int, value0: java.lang.CharSequence) extends IndexedValue
  case class Obj(index: Int, value0: (java.lang.CharSequence, IndexedValue)*) extends IndexedValue
  case class Arr(index: Int, value: IndexedValue*) extends IndexedValue
  case class Num(index: Int, s: CharSequence, decIndex: Int, expIndex: Int) extends IndexedValue
  case class NumRaw(index: Int, d: Double) extends IndexedValue
  case class False(index: Int) extends IndexedValue{
    def value = false
  }
  case class True(index: Int) extends IndexedValue{
    def value = true
  }
  case class Null(index: Int) extends IndexedValue{
    def value = null
  }

  def transform[T](j: IndexedValue, f: Visitor[_, T]): T = try{
    j match{
      case IndexedValue.Null(i) => f.visitNull(i)
      case IndexedValue.True(i) => f.visitTrue(i)
      case IndexedValue.False(i) => f.visitFalse(i)
      case IndexedValue.Str(i, s) => f.visitString(s, i)
      case IndexedValue.Num(i, s, d, e) => f.visitFloat64StringParts(s, d, e, i)
      case IndexedValue.NumRaw(i, d) => f.visitFloat64(d, i)
      case IndexedValue.Arr(i, items @_*) =>
        val ctx = f.visitArray(-1, -1).narrow
        for(item <- items) try ctx.visitValue(transform(item, ctx.subVisitor), item.index) catch reject(item.index)
        ctx.visitEnd(i)
      case IndexedValue.Obj(i, items @_*) =>
        val ctx = f.visitObject(-1, -1).narrow
        for((k, item) <- items) {
          val keyVisitor = try ctx.visitKey(i) catch reject(i)

          ctx.visitKeyValue(keyVisitor.visitString(k, i))
          try ctx.visitValue(transform(item, ctx.subVisitor), item.index) catch reject(item.index)
        }
        ctx.visitEnd(i)
    }
  } catch reject(j.index)


  object Builder extends JsVisitor[IndexedValue, IndexedValue]{
    def visitArray(length: Int, i: Int) = new ArrVisitor[IndexedValue, IndexedValue.Arr] {
      val out = mutable.Buffer.empty[IndexedValue]
      def subVisitor = Builder
      def visitValue(v: IndexedValue, index: Int): Unit = {
        out.append(v)
      }
      def visitEnd(index: Int): IndexedValue.Arr = IndexedValue.Arr(i, out.toSeq:_*)
    }

    def visitObject(length: Int, i: Int) = new ObjVisitor[IndexedValue, IndexedValue.Obj] {
      val out = mutable.Buffer.empty[(String, IndexedValue)]
      var currentKey: String = _
      def subVisitor = Builder
      def visitKey(index: Int) = IndexedValue.Builder
      def visitKeyValue(s: Any): Unit = currentKey = s.asInstanceOf[IndexedValue.Str].value0.toString
      def visitValue(v: IndexedValue, index: Int): Unit = {
        out.append((currentKey, v))
      }
      def visitEnd(index: Int): IndexedValue.Obj = IndexedValue.Obj(i, out.toSeq:_*)
    }

    def visitNull(i: Int) = IndexedValue.Null(i)

    def visitFalse(i: Int) = IndexedValue.False(i)

    def visitTrue(i: Int) = IndexedValue.True(i)

    def visitFloat64StringParts(s: CharSequence, decIndex: Int, expIndex: Int, i: Int) = IndexedValue.Num(i, s.toString, decIndex, expIndex)
    override def visitFloat64(d: Double, i: Int) = IndexedValue.NumRaw(i, d)

    def visitString(s: CharSequence, i: Int) = IndexedValue.Str(i, s.toString)
  }
}
