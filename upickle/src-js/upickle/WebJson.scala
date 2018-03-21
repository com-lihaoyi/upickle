package upickle

import upickle.internal.IndexedJs
import upickle.jawn.{AbortJsonProcessingException, JsonProcessingException, RawFContext}

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
  def visit[T](j: js.Any, f: upickle.jawn.RawFacade[_, T]): T = {
    (j: Any) match{
      case s: String => f.jstring(s, -1)
      case n: Double =>
        val s = n.toString
        f.jnum(s, s.indexOf('.'), s.indexOf('E'), -1)
      case true => f.jtrue(-1)
      case false => f.jfalse(-1)
      case null => f.jnull(-1)
      case s: js.Array[js.Any] =>
        val ctx = f.arrayContext(-1).asInstanceOf[RawFContext[Any, T]]
        for(i <- s) ctx.add(visit(i, ctx.facade), -1)
        ctx.finish(-1)
      case s: js.Object =>
        val ctx = f.objectContext(-1).asInstanceOf[RawFContext[Any, T]]
        for(p <- s.asInstanceOf[js.Dictionary[js.Any]]) {
          ctx.visitKey(p._1, -1)
          ctx.add(visit(p._2, ctx.facade), -1)
        }
        ctx.finish(-1)
    }
  }

  object Builder extends upickle.jawn.Facade[js.Any]{
    def singleContext() = ???

    def arrayContext() = new RawFContext[js.Any, js.Any] {
      val out = new js.Array[js.Any]
      def facade = Builder.this
      def visitKey(s: CharSequence, index: Int): Unit = ???
      def add(v: js.Any, index: Int): Unit = out.append(v)

      def finish(index: Int): js.Any = out
      def isObj = false
    }

    def objectContext() = new RawFContext[js.Any, js.Any] {
      val out = js.Dictionary[js.Any]()
      var currentKey: String = _
      def facade = Builder.this
      def visitKey(s: CharSequence, index: Int): Unit = currentKey = s.toString
      def add(v: js.Any, index: Int): Unit = {
        out(currentKey) = v
      }
      def finish(index: Int) = out
      def isObj = true
    }

    def jnull() = null.asInstanceOf[js.Any]

    def jfalse() = false.asInstanceOf[js.Any]

    def jtrue() = true.asInstanceOf[js.Any]

    def jnum(s: CharSequence, decIndex: Int, expIndex: Int) = {
      s.toString.toDouble.asInstanceOf[js.Any]
    }

    def jstring(s: CharSequence) = s.toString.asInstanceOf[js.Any]
  }
}

