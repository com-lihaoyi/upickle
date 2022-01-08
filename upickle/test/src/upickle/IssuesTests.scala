package upickle

import upickle.default._
import utest._

object IssuesTests extends TestSuite {
  case class Node(head: String, tail: Option[Node])
  object Node {
    implicit val nodeRW: ReadWriter[Node] = macroRW[Node]
  }
  val tests = Tests {
    test("issue-#371") {
      val input = """{"head":"a","tail":[{"head":"b","tail":[]}]}"""
      val expected = Node("a", Some(Node("b", None)))
      val result = read[Node](input)
      assert(result == expected)
    }
  }
}
