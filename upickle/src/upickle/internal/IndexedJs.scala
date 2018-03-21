package upickle.internal

import upickle.jawn.{ArrVisitor, ObjArrVisitor, ObjVisitor, Walker}
import upickle.visitors.JsVisitor.{reject, visit}

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
object IndexedJs extends Walker[IndexedJs]{
  
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

  def visit[T](j: IndexedJs, f: upickle.jawn.Visitor[_, T]): T = try{
    j match{
      case IndexedJs.Null(i) => f.jnull(i)
      case IndexedJs.True(i) => f.jtrue(i)
      case IndexedJs.False(i) => f.jfalse(i)
      case IndexedJs.Str(i, s) => f.jstring(s, i)
      case IndexedJs.Num(i, s, d, e) => f.jnum(s, d, e, i)
      case IndexedJs.Arr(i, items @_*) =>
        val ctx = f.arrayContext(-1).narrow
        for(item <- items) try ctx.add(visit(item, ctx.subVisitor), item.index) catch reject(item.index, Nil)
        ctx.finish(i)
      case IndexedJs.Obj(i, items @_*) =>
        val ctx = f.objectContext(-1).narrow
        for((k, item) <- items) {
          try ctx.visitKey(k, i) catch reject(i, Nil)
          try ctx.add(visit(item, ctx.subVisitor), item.index) catch reject(item.index, Nil)
        }
        ctx.finish(i)
    }
  } catch reject(j.index, Nil)


  object Builder extends upickle.jawn.Visitor[IndexedJs, IndexedJs]{
    def singleContext(i: Int) = ???

    def arrayContext(i: Int) = new ArrVisitor[IndexedJs, IndexedJs] {
      val out = mutable.Buffer.empty[IndexedJs]
      def subVisitor = Builder.this
      def add(v: IndexedJs, index: Int): Unit = {
        out.append(v)
      }
      def finish(index: Int): IndexedJs = IndexedJs.Arr(i, out:_*)
    }

    def objectContext(i: Int) = new ObjVisitor[IndexedJs, IndexedJs] {
      val out = mutable.Buffer.empty[(String, IndexedJs)]
      var currentKey: String = _
      def subVisitor = Builder.this
      def visitKey(s: CharSequence, index: Int): Unit = currentKey = s.toString
      def add(v: IndexedJs, index: Int): Unit = {
        out.append((currentKey, v))
      }
      def finish(index: Int): IndexedJs = IndexedJs.Obj(i, out:_*)
    }

    def jnull(i: Int) = IndexedJs.Null(i)

    def jfalse(i: Int) = IndexedJs.False(i)

    def jtrue(i: Int) = IndexedJs.True(i)

    def jnum(s: CharSequence, decIndex: Int, expIndex: Int, i: Int) = IndexedJs.Num(i, s, decIndex, expIndex)

    def jstring(s: CharSequence, i: Int) = IndexedJs.Str(i, s)
  }
}
