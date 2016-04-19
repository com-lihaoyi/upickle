package test.pprint

import derive.GenericADTs.DeltaInvariant
import pprint.Config.Defaults._
import utest._

object Only211Tests extends TestSuite{
  val tests = TestSuite{
    'delta {
      Check(
        DeltaInvariant.Clear(): DeltaInvariant[Nothing, Nothing],
        """Clear()"""
      )
    }

    'exponential{
      import derive.Exponential._

      Check(
        A1(A2(A3(null, null), null), A2(null, A3(A4(null, null), null))),
        "A1(A2(A3(null, null), null), A2(null, A3(A4(null, null), null)))"
      )
    }
    'recursive {
      import derive.Recursive._
      Check(
        IntTree(1, List(IntTree(2, List()), IntTree(4, List(IntTree(3, List()))))),
        "IntTree(1, List(IntTree(2, List()), IntTree(4, List(IntTree(3, List())))))"
      )
    }
  }
}
