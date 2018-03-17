package upickle
package visitors

import upickle.internal.IndexedJs
import upickle.jawn.{AbortJsonProcessingException, JsonProcessingException, RawFContext}

import scala.collection.mutable

object JsVisitor {
  def visit[T](j: Js.Value, f: upickle.jawn.RawFacade[_, T]): T = {
    j match{
      case Js.Null => f.jnull(-1)
      case Js.True => f.jtrue(-1)
      case Js.False => f.jfalse(-1)
      case Js.Str(s) => f.jstring(s, -1)
      case Js.Num(d) => f.jnum(d.toString, -1, -1, -1)
      case Js.Arr(items @ _*) =>
        val ctx = f.arrayContext(-1).asInstanceOf[RawFContext[Any, T]]
        for(item <- items) ctx.add(visit(item, ctx.facade), -1)
        ctx.finish(-1)
      case Js.Obj(items @ _*) =>
        val ctx = f.objectContext(-1).asInstanceOf[RawFContext[Any, T]]
        for((k, item) <- items) {
          ctx.visitKey(k, -1)
          ctx.add(visit(item, ctx.facade), -1)
        }
        ctx.finish(-1)
    }
  }
  def reject(j: Int, path: List[Any]): PartialFunction[Throwable, Nothing] = {
    case e: AbortJsonProcessingException =>

      throw new JsonProcessingException(e.msg, j, -1, -1, path)
  }
  def visit[T](j: IndexedJs.Value, f: upickle.jawn.RawFacade[_, T]): T = try{
    j match{
      case IndexedJs.Null(i) => f.jnull(i)
      case IndexedJs.True(i) => f.jtrue(i)
      case IndexedJs.False(i) => f.jfalse(i)
      case IndexedJs.Str(i, s) => f.jstring(s, i)
      case IndexedJs.Num(i, s, d, e) => f.jnum(s, d, e, i)
      case IndexedJs.Arr(i, items @_*) =>
        val ctx = f.arrayContext(-1).asInstanceOf[RawFContext[Any, T]]
        for(item <- items) try ctx.add(visit(item, ctx.facade), item.index) catch reject(item.index, Nil)
        ctx.finish(i)
      case IndexedJs.Obj(i, items @_*) =>
        val ctx = f.objectContext(-1).asInstanceOf[RawFContext[Any, T]]
        for((k, item) <- items) {
          try ctx.visitKey(k, i) catch reject(i, Nil)
          try ctx.add(visit(item, ctx.facade), item.index) catch reject(item.index, Nil)
        }
        ctx.finish(i)
    }
  } catch reject(j.index, Nil)
}


object JsBuilder extends upickle.jawn.Facade[Js.Value]{
  def singleContext() = ???

  def arrayContext() = new RawFContext[Js.Value, Js.Value] {
    val out = mutable.Buffer.empty[Js.Value]
    def facade = JsBuilder.this
    def visitKey(s: CharSequence, index: Int): Unit = ???
    def add(v: Js.Value, index: Int): Unit = {
      out.append(v)
    }
    def finish(index: Int): Js.Value = Js.Arr(out:_*)
    def isObj = false
  }

  def objectContext() = new RawFContext[Js.Value, Js.Value] {
    val out = mutable.Buffer.empty[(String, Js.Value)]
    var currentKey: String = _
    def facade = JsBuilder.this
    def visitKey(s: CharSequence, index: Int): Unit = currentKey = s.toString
    def add(v: Js.Value, index: Int): Unit = {
      out.append((currentKey, v))
    }
    def finish(index: Int): Js.Value = Js.Obj(out:_*)
    def isObj = true
  }

  def jnull() = Js.Null

  def jfalse() = Js.False

  def jtrue() = Js.True

  def jnum(s: CharSequence, decIndex: Int, expIndex: Int) = Js.Num(s.toString.toDouble)

  def jstring(s: CharSequence) = Js.Str(s)
}

object IndexedJsBuilder extends upickle.jawn.RawFacade[IndexedJs.Value, IndexedJs.Value]{
  def singleContext(i: Int) = ???

  def arrayContext(i: Int) = new RawFContext[IndexedJs.Value, IndexedJs.Value] {
    val out = mutable.Buffer.empty[IndexedJs.Value]
    def facade = IndexedJsBuilder.this
    def visitKey(s: CharSequence, index: Int): Unit = ???
    def add(v: IndexedJs.Value, index: Int): Unit = {
      out.append(v)
    }
    def finish(index: Int): IndexedJs.Value = IndexedJs.Arr(i, out:_*)
    def isObj = false
  }

  def objectContext(i: Int) = new RawFContext[IndexedJs.Value, IndexedJs.Value] {
    val out = mutable.Buffer.empty[(String, IndexedJs.Value)]
    var currentKey: String = _
    def facade = IndexedJsBuilder.this
    def visitKey(s: CharSequence, index: Int): Unit = currentKey = s.toString
    def add(v: IndexedJs.Value, index: Int): Unit = {
      out.append((currentKey, v))
    }
    def finish(index: Int): IndexedJs.Value = IndexedJs.Obj(i, out:_*)
    def isObj = true
  }

  def jnull(i: Int) = IndexedJs.Null(i)

  def jfalse(i: Int) = IndexedJs.False(i)

  def jtrue(i: Int) = IndexedJs.True(i)

  def jnum(s: CharSequence, decIndex: Int, expIndex: Int, i: Int) = IndexedJs.Num(i, s, decIndex, expIndex)

  def jstring(s: CharSequence, i: Int) = IndexedJs.Str(i, s)
}