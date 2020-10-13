package upickle.implicits

import deriving.Mirror
import scala.reflect.ClassTag

trait MacroImplicits extends Readers with Writers with upickle.core.Annotator:
  this: upickle.core.Types =>

  inline def macroRW[T: ClassTag](using Mirror.Of[T]): ReadWriter[T] =
    ReadWriter.join(
      macroR[T],
      macroW[T]
    )
  end macroRW

end MacroImplicits
