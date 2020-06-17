package upickle

import upickle.core.Visitor
import upickle.implicits.MacroImplicits


trait MsgReadWriters extends upickle.core.Types with MacroImplicits {
  implicit val MsgValueR: Reader[upack.Msg] = new Reader.Delegate(upack.Msg)

  implicit val MsgValueW: Writer[upack.Msg] = new Writer[upack.Msg] {
    def write0[R](out: Visitor[_, R], v: upack.Msg): R = upack.transform(v, out)
  }
}