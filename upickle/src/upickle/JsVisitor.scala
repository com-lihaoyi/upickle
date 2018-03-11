package upickle

import jawn.RawFContext

import scala.collection.mutable

object JsVisitor {
  def visit[T](j: Js.Value, f: jawn.RawFacade[_, T]): T = {
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
}


object JsBuilder extends jawn.Facade[Js.Value]{
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