package upickle.core

import utest._

// Needed because of a bug in mill scala native module
// crashing when there are no tests in a module1.

object EmptyTest extends TestSuite {
  val tests = Tests {

  }
}
