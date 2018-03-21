package upickle

import upickle.internal.IndexedJs
import upickle.jawn._

import scala.collection.mutable
import scala.scalajs.js

trait WebJson extends upickle.core.Types {
  object web{
    def read[T: Reader](s: String) = {
      WebJson.visit(js.JSON.parse(s), implicitly[Reader[T]])
    }

    def write[T: Writer](t: T, indent: Int = -1) = {
      js.JSON.stringify(implicitly[Writer[T]].write(WebJson.Builder, t))
    }
  }
}
object WebJson extends jawn.Walker[js.Any]{
  def visit[T](j: js.Any, f: upickle.jawn.Visitor[_, T]): T = {
    (j: Any) match{
      case s: String => f.jstring(s, -1)
      case n: Double =>
        val s = n.toString
        f.jnum(s, s.indexOf('.'), s.indexOf('E'), -1)
      case true => f.jtrue(-1)
      case false => f.jfalse(-1)
      case null => f.jnull(-1)
      case s: js.Array[js.Any] =>
        val ctx = f.arrayContext(-1).narrow
        for(i <- s) ctx.add(visit(i, ctx.subVisitor), -1)
        ctx.finish(-1)
      case s: js.Object =>
        val ctx = f.objectContext(-1).narrow
        for(p <- s.asInstanceOf[js.Dictionary[js.Any]]) {
          ctx.visitKey(p._1, -1)
          ctx.add(visit(p._2, ctx.subVisitor), -1)
        }
        ctx.finish(-1)
    }
  }

  object Builder extends upickle.jawn.Visitor[js.Any, js.Any]{
    def arrayContext(index: Int) = new ArrVisitor[js.Any, js.Any] {
      val out = new js.Array[js.Any]
      def subVisitor = Builder.this
      def add(v: js.Any, index: Int): Unit = out.append(v)

      def finish(index: Int): js.Any = out
    }

    def objectContext(index: Int) = new ObjVisitor[js.Any, js.Any] {
      val out = js.Dictionary[js.Any]()
      var currentKey: String = _
      def subVisitor = Builder.this
      def visitKey(s: CharSequence, index: Int): Unit = currentKey = s.toString
      def add(v: js.Any, index: Int): Unit = {
        out(currentKey) = v
      }
      def finish(index: Int) = out
    }

    def jnull(index: Int) = null.asInstanceOf[js.Any]

    def jfalse(index: Int) = false.asInstanceOf[js.Any]

    def jtrue(index: Int) = true.asInstanceOf[js.Any]

    def jnum(s: CharSequence, decIndex: Int, expIndex: Int, index: Int) = {
      s.toString.toDouble.asInstanceOf[js.Any]
    }

    def jstring(s: CharSequence, index: Int) = s.toString.asInstanceOf[js.Any]
  }
}

