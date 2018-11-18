package ujson

import ujson._
import upickle.core.{ArrVisitor, ObjVisitor}

import scala.scalajs.js

object WebJson extends ujson.Transformer[js.Any]{
  def transform[T](j: js.Any, f: upickle.core.Visitor[_, T]): T = {
    (j: Any) match{
      case s: String => f.visitString(s, -1)
      case n: Double => f.visitFloat64(n, -1)
      case true => f.visitTrue(-1)
      case false => f.visitFalse(-1)
      case null => f.visitNull(-1)
      case s: js.Array[js.Any] =>
        val ctx = f.visitArray(-1, -1).narrow
        for(i <- s) ctx.visitValue(transform(i, ctx.subVisitor), -1)
        ctx.visitEnd(-1)
      case s: js.Object =>
        val ctx = f.visitObject(-1, -1).narrow
        for(p <- s.asInstanceOf[js.Dictionary[js.Any]]) {
          val keyVisitor = ctx.visitKey(-1)
          ctx.visitKeyValue(transform(p._1, keyVisitor))
          ctx.visitValue(p._2, -1)
        }
        ctx.visitEnd(-1)
    }
  }

  object Builder extends JsVisitor[js.Any, js.Any]{
    def visitArray(length: Int, index: Int) = new ArrVisitor[js.Any, js.Any] {
      val out = new js.Array[js.Any]
      def subVisitor = Builder.this
      def visitValue(v: js.Any, index: Int): Unit = out.append(v)

      def visitEnd(index: Int): js.Any = out
    }

    def visitObject(length: Int, index: Int) = new ObjVisitor[js.Any, js.Any] {
      val out = js.Dictionary[js.Any]()
      var currentKey: String = _
      def subVisitor = Builder.this
      def visitKey(index: Int) = upickle.core.StringVisitor
      def visitKeyValue(s: Any): Unit = currentKey = s.toString
      def visitValue(v: js.Any, index: Int): Unit = {
        out(currentKey) = v
      }
      def visitEnd(index: Int) = out
    }

    def visitNull(index: Int) = null.asInstanceOf[js.Any]

    def visitFalse(index: Int) = false.asInstanceOf[js.Any]

    def visitTrue(index: Int) = true.asInstanceOf[js.Any]

    def visitFloat64StringParts(s: CharSequence, decIndex: Int, expIndex: Int, index: Int) = {
      s.toString.toDouble.asInstanceOf[js.Any]
    }

    override def visitFloat64(d: Double, index: Int) = {
      d.asInstanceOf[js.Any]
    }

    def visitString(s: CharSequence, index: Int) = s.toString.asInstanceOf[js.Any]
  }
}

