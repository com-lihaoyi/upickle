package upickle.implicits

import upickle.core.Annotator

trait WritersVersionSpecific extends CaseClassWriterPiece:
  this: upickle.core.Types with Writers with Annotator =>
end WritersVersionSpecific