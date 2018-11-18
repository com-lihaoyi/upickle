package upickle

import upickle.core.Visitor
import upickle.implicits.MacroImplicits

trait MsgReadWriters extends upickle.core.Types with MacroImplicits{
  implicit val MsgValueR: Reader[upack.Msg] = new Reader[upack.Msg]{
    override def expectedMsg = "JSON value"
    override def visitObject(length: Int, index: Int) = upack.Msg.visitObject(-1, index).narrow
    override def visitArray(length: Int, index: Int) = upack.Msg.visitArray(-1, index).narrow
    override def visitString(s: CharSequence, index: Int) = upack.Msg.visitString(s, index)
    override def visitFloat64StringParts(s: CharSequence, decIndex: Int, expIndex: Int, index: Int) = {
      upack.Msg.visitFloat64StringParts(s, decIndex, expIndex, index)
    }
    override def visitFloat64(d: Double, index: Int) = upack.Msg.visitFloat64(d, index)
    
    override def visitTrue(index: Int) = upack.Msg.visitTrue(index)
    override def visitFalse(index: Int) = upack.Msg.visitFalse(index)
    override def visitNull(index: Int) = upack.Msg.visitNull(index)

    override def visitInt32(i: Int, index: Int) = upack.Msg.visitInt32(i, index)
    override def visitInt64(i: Long, index: Int) = upack.Msg.visitInt64(i, index)

    override def visitUInt64(i: Long, index: Int) = upack.Msg.visitUInt64(i, index)
    override def visitChar(i: Char, index: Int) = upack.Msg.visitChar(i, index)
  }

  implicit val MsgValueW: Writer[upack.Msg] = new Writer[upack.Msg] {
    def write0[R](out: Visitor[_, R], v: upack.Msg): R = upack.transform(v, out)
  }
}