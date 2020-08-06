package upickle.implicits

import deriving.Mirror
import scala.reflect.ClassTag

// This trait is needed only for
// JsReadWriters and MsgReadWriters
// to take priority over macro implicits.
// TODO come up with a better design without
// this dummy trait
trait MacroImplicits extends Readers with Writers with upickle.core.Annotator:
  this: upickle.core.Types =>

  inline def macroRW[T: ClassTag](using Mirror.Of[T]): ReadWriter[T] =
    ReadWriter.join(
      macroR[T],
      macroW[T]
    )
  end macroRW

end MacroImplicits
