package ujson

import upickle.core.NoOpVisitor

import scala.util.Try

class FileSyntaxCheck extends SyntaxCheck {
  override def isValidSyntax(s: String): Boolean = {
    TestUtil.withTemp(s) { t =>
      Try(Readable.fromFile(t).transform(NoOpVisitor)).isSuccess
    }
  }
}
