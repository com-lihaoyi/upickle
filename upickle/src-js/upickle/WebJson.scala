package upickle

import upickle.json._

import scala.scalajs.js

trait WebJson extends upickle.core.Types {
  object web {
    def read[T: Reader](s: String) = {
      WebJson.transform(js.JSON.parse(s), implicitly[Reader[T]])
    }

    def write[T: Writer](t: T, indent: Int = -1) = {
      js.JSON.stringify(implicitly[Writer[T]].write(WebJson.Builder, t))
    }
  }
}
object WebJson extends json.Transformer[js.Any]{
  def transform[T](j: js.Any, f: upickle.json.Visitor[_, T]): T = {
    (j: Any) match{
      case s: String => f.visitString(s, -1)
      case n: Double => f.visitNumRaw(n, -1)
      case true => f.visitTrue(-1)
      case false => f.visitFalse(-1)
      case null => f.visitNull(-1)
      case s: js.Array[js.Any] =>
        val ctx = f.visitArray(-1).narrow
        for(i <- s) ctx.visitValue(transform(i, ctx.subVisitor), -1)
        ctx.visitEnd(-1)
      case s: js.Object =>
        val ctx = f.visitObject(-1).narrow
        for(p <- s.asInstanceOf[js.Dictionary[js.Any]]) {
          ctx.visitKey(p._1, -1)
          ctx.visitValue(transform(p._2, ctx.subVisitor), -1)
        }
        ctx.visitEnd(-1)
    }
  }

  object Builder extends upickle.json.Visitor[js.Any, js.Any]{
    def visitArray(index: Int) = new ArrVisitor[js.Any, js.Any] {
      val out = new js.Array[js.Any]
      def subVisitor = Builder.this
      def visitValue(v: js.Any, index: Int): Unit = out.append(v)

      def visitEnd(index: Int): js.Any = out
    }

    def visitObject(index: Int) = new ObjVisitor[js.Any, js.Any] {
      val out = js.Dictionary[js.Any]()
      var currentKey: String = _
      def subVisitor = Builder.this
      def visitKey(s: CharSequence, index: Int): Unit = currentKey = s.toString
      def visitValue(v: js.Any, index: Int): Unit = {
        out(currentKey) = v
      }
      def visitEnd(index: Int) = out
    }

    def visitNull(index: Int) = null.asInstanceOf[js.Any]

    def visitFalse(index: Int) = false.asInstanceOf[js.Any]

    def visitTrue(index: Int) = true.asInstanceOf[js.Any]

    def visitNum(s: CharSequence, decIndex: Int, expIndex: Int, index: Int) = {
      s.toString.toDouble.asInstanceOf[js.Any]
    }

    override def visitNumRaw(d: Double, index: Int) = {
      d.asInstanceOf[js.Any]
    }

    def visitString(s: CharSequence, index: Int) = s.toString.asInstanceOf[js.Any]
  }
}

