package upickle
package json
package parser

import org.scalatest._
import prop._
import org.scalacheck.Arbitrary._
import org.scalacheck._
import Gen._
import Arbitrary.arbitrary

import scala.util.{Try, Success, Failure}

class FileSyntaxCheck extends SyntaxCheck {
  override def isValidSyntax(s: String): Boolean = {
    TestUtil.withTemp(s) { t =>
      Try(Transformable.fromFile(t).transform(NoOpVisitor)).isSuccess
    }
  }
}
