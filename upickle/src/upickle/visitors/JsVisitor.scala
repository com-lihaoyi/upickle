package upickle
package visitors

import upickle.internal.IndexedJs
import upickle.jawn._

import scala.collection.mutable


object JsVisitor extends jawn.Walker[Js.Value]{
  def walk[T](j: Js.Value, f: upickle.jawn.Visitor[_, T]): T = {
    j match{
      case Js.Null => f.visitNull(-1)
      case Js.True => f.visitTrue(-1)
      case Js.False => f.visitFalse(-1)
      case Js.Str(s) => f.visitString(s, -1)
      case Js.Num(d) => f.visitNum(d.toString, -1, -1, -1)
      case Js.Arr(items @ _*) =>
        val ctx = f.visitArray(-1).narrow
        for(item <- items) ctx.visitValue(walk(item, ctx.subVisitor), -1)
        ctx.visitEnd(-1)
      case Js.Obj(items @ _*) =>
        val ctx = f.visitObject(-1).narrow
        for((k, item) <- items) {
          ctx.visitKey(k, -1)
          ctx.visitValue(walk(item, ctx.subVisitor), -1)
        }
        ctx.visitEnd(-1)
    }
  }
  def reject(j: Int, path: List[Any]): PartialFunction[Throwable, Nothing] = {
    case e: AbortJsonProcessingException =>

      throw new JsonProcessingException(e.msg, j, -1, -1, path, e)
  }
}


object JsBuilder extends upickle.jawn.Visitor[Js.Value, Js.Value]{
  def visitArray(index: Int) = new ArrVisitor[Js.Value, Js.Value] {
    val out = mutable.Buffer.empty[Js.Value]
    def subVisitor = JsBuilder.this
    def visitValue(v: Js.Value, index: Int): Unit = {
      out.append(v)
    }
    def visitEnd(index: Int): Js.Value = Js.Arr(out:_*)
  }

  def visitObject(index: Int) = new ObjVisitor[Js.Value, Js.Value] {
    val out = mutable.Buffer.empty[(String, Js.Value)]
    var currentKey: String = _
    def subVisitor = JsBuilder.this
    def visitKey(s: CharSequence, index: Int): Unit = currentKey = s.toString
    def visitValue(v: Js.Value, index: Int): Unit = {
      out.append((currentKey, v))
    }
    def visitEnd(index: Int): Js.Value = Js.Obj(out:_*)
  }

  def visitNull(index: Int) = Js.Null

  def visitFalse(index: Int) = Js.False

  def visitTrue(index: Int) = Js.True

  def visitNum(s: CharSequence, decIndex: Int, expIndex: Int, index: Int) = {
    Js.Num(s.toString.toDouble)
  }

  def visitString(s: CharSequence, index: Int) = Js.Str(s)
}
