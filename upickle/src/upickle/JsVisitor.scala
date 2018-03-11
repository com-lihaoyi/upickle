package upickle

import jawn.RawFContext

object JsVisitor {
  def visit[T](j: Js.Value, f: jawn.RawFacade[T]): T = {
    j match{
      case Js.Null => f.jnull(-1)
      case Js.True => f.jtrue(-1)
      case Js.False => f.jfalse(-1)
      case Js.Str(s) => f.jstring(s, -1)
      case Js.Num(d) => f.jnum(d.toString, -1, -1, -1)
      case Js.Arr(items @ _*) =>
        val ctx = f.arrayContext(-1).asInstanceOf[RawFContext[Any, T]]
        for(item <- items) ctx.add(item, -1)
        ctx.finish(-1)
      case Js.Obj(items @ _*) =>
        val ctx = f.objectContext(-1).asInstanceOf[RawFContext[Any, T]]
        for((k, item) <- items) {
          ctx.visitKey(k, -1)
          ctx.add(item, -1)
        }
        ctx.finish(-1)
    }
  }
}
