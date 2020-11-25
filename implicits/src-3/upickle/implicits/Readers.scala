package upickle.implicits

import upickle.core.{ Visitor, ObjVisitor, Annotator }

import deriving._, compiletime._

trait ReadersVersionSpecific extends CaseClassReaderPiece:
  this: upickle.core.Types with Readers with Annotator =>
end ReadersVersionSpecific
