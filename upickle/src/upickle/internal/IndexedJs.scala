package upickle.internal

import upickle.json.{ArrVisitor, ObjArrVisitor, ObjVisitor, Transformer}
import upickle.json.Js.reject

import scala.collection.mutable

/**
  * A version of [[upickle.Js]] that keeps the index positions of the various AST
  * nodes it is constructing. Usually not necessary, but sometimes useful if you
  * want to work with an AST but still provide source-index error positions if
  * something goes wrong
  */
sealed trait IndexedJs {
  def index: Int
}
object IndexedJs extends Transformer[IndexedJs]{
  
  case class Str(index: Int, value0: java.lang.CharSequence) extends IndexedJs
  case class Obj(index: Int, value0: (java.lang.CharSequence, IndexedJs)*) extends IndexedJs
  case class Arr(index: Int, value: IndexedJs*) extends IndexedJs
  case class Num(index: Int, s: CharSequence, decIndex: Int, expIndex: Int) extends IndexedJs
  case class False(index: Int) extends IndexedJs{
    def value = false
  }
  case class True(index: Int) extends IndexedJs{
    def value = true
  }
  case class Null(index: Int) extends IndexedJs{
    def value = null
  }

  def transform[T](j: IndexedJs, f: upickle.json.Visitor[_, T]): T = try{
    j match{
      case IndexedJs.Null(i) => f.visitNull(i)
      case IndexedJs.True(i) => f.visitTrue(i)
      case IndexedJs.False(i) => f.visitFalse(i)
      case IndexedJs.Str(i, s) => f.visitString(s, i)
      case IndexedJs.Num(i, s, d, e) => f.visitNum(s, d, e, i)
      case IndexedJs.Arr(i, items @_*) =>
        val ctx = f.visitArray(-1).narrow
        for(item <- items) try ctx.visitValue(transform(item, ctx.subVisitor), item.index) catch reject(item.index, Nil)
        ctx.visitEnd(i)
      case IndexedJs.Obj(i, items @_*) =>
        val ctx = f.visitObject(-1).narrow
        for((k, item) <- items) {
          try ctx.visitKey(k, i) catch reject(i, Nil)
          try ctx.visitValue(transform(item, ctx.subVisitor), item.index) catch reject(item.index, Nil)
        }
        ctx.visitEnd(i)
    }
  } catch reject(j.index, Nil)


  object Builder extends upickle.json.Visitor[IndexedJs, IndexedJs]{
    def visitArray(i: Int) = new ArrVisitor[IndexedJs, IndexedJs.Arr] {
      val out = mutable.Buffer.empty[IndexedJs]
      def subVisitor = Builder.this
      def visitValue(v: IndexedJs, index: Int): Unit = {
        out.append(v)
      }
      def visitEnd(index: Int): IndexedJs.Arr = IndexedJs.Arr(i, out:_*)
    }

    def visitObject(i: Int) = new ObjVisitor[IndexedJs, IndexedJs.Obj] {
      val out = mutable.Buffer.empty[(String, IndexedJs)]
      var currentKey: String = _
      def subVisitor = Builder.this
      def visitKey(s: CharSequence, index: Int): Unit = currentKey = s.toString
      def visitValue(v: IndexedJs, index: Int): Unit = {
        out.append((currentKey, v))
      }
      def visitEnd(index: Int): IndexedJs.Obj = IndexedJs.Obj(i, out:_*)
    }

    def visitNull(i: Int) = IndexedJs.Null(i)

    def visitFalse(i: Int) = IndexedJs.False(i)

    def visitTrue(i: Int) = IndexedJs.True(i)

    def visitNum(s: CharSequence, decIndex: Int, expIndex: Int, i: Int) = IndexedJs.Num(i, s, decIndex, expIndex)

    def visitString(s: CharSequence, i: Int) = IndexedJs.Str(i, s)
  }
}
