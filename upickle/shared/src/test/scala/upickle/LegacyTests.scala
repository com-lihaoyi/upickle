package upickle
import utest._
import derive._
import LegacyTestUtil.rw
object LegacyTests extends TestSuite{
  val tests = TestSuite{
    'sealedHierarchy {
      // objects in sealed case class hierarchies should always read and write
      // the same way (with a tag) regardless of what their static type is when
      // written. This is feasible because sealed hierarchies can only have a
      // finite number of cases, so we can just check them all and decide which
      // class the instance belongs to.
      import Hierarchy._
      'shallow {
        * - rw(B(1), """["derive.Hierarchy.B",{"i":1}]""")
        * - rw(C("a", "b"), """["derive.Hierarchy.C",{"s1":"a","s2":"b"}]""")
        //Doesn't work in 2.10.4
        //          * - rw(AnZ: Z, """["derive.Hierarchy.AnZ",{}]""")
        //          * - rw(AnZ, """["derive.Hierarchy.AnZ",{}]""")

        * - rw(Hierarchy.B(1): Hierarchy.A, """["derive.Hierarchy.B",{"i":1}]""")
        * - rw(C("a", "b"): A, """["derive.Hierarchy.C",{"s1":"a","s2":"b"}]""")
      }
      'deep{
        import DeepHierarchy._

        * - rw(B(1), """["derive.DeepHierarchy.B",{"i":1}]""")
        * - rw(B(1): A, """["derive.DeepHierarchy.B",{"i":1}]""")
        * - rw(AnQ(1): Q, """["derive.DeepHierarchy.AnQ",{"i":1}]""")
        * - rw(AnQ(1), """["derive.DeepHierarchy.AnQ",{"i":1}]""")

        * - rw(F(AnQ(1)), """["derive.DeepHierarchy.F",{"q":["derive.DeepHierarchy.AnQ",{"i":1}]}]""")
        * - rw(F(AnQ(2)): A, """["derive.DeepHierarchy.F",{"q":["derive.DeepHierarchy.AnQ",{"i":2}]}]""")
        * - rw(F(AnQ(3)): C, """["derive.DeepHierarchy.F",{"q":["derive.DeepHierarchy.AnQ",{"i":3}]}]""")
        * - rw(D("1"), """["derive.DeepHierarchy.D",{"s":"1"}]""")
        * - rw(D("1"): C, """["derive.DeepHierarchy.D",{"s":"1"}]""")
        * - rw(D("1"): A, """["derive.DeepHierarchy.D",{"s":"1"}]""")
        * - rw(E(true), """["derive.DeepHierarchy.E",{"b":true}]""")
        * - rw(E(true): C, """["derive.DeepHierarchy.E",{"b":true}]""")
        * - rw(E(true): A, """["derive.DeepHierarchy.E",{"b":true}]""")
      }
    }
    'singleton {
      import Singletons._

      rw(BB, """["derive.Singletons.BB",{}]""")
      rw(CC, """["derive.Singletons.CC",{}]""")
      rw(BB: AA, """["derive.Singletons.BB",{}]""")
      rw(CC: AA, """["derive.Singletons.CC",{}]""")
    }
    'ADT{
      import GenericADTs._
      * - {
        val pref1 = "derive.GenericADTs.Delta"
        val D1 = Delta
        type D1[+A, +B] = Delta[A, B]
        rw(D1.Insert(1, 1), s"""["$pref1.Insert",{"key":1,"value":1}]""")
        rw(D1.Insert(1, 1): D1[Int, Int], s"""["$pref1.Insert",{"key":1,"value":1}]""")
        rw(D1.Remove(1), s"""["$pref1.Remove",{"key":1}]""")
        rw(D1.Remove(1): D1[Int, Int], s"""["$pref1.Remove",{"key":1}]""")
        rw(D1.Clear(), s"""["$pref1.Clear",{}]""")
        rw(D1.Clear(): D1[Int, Int], s"""["$pref1.Clear",{}]""")
      }
      * - {
        val pref2 = "derive.GenericADTs.DeltaInvariant"
        val D2 = DeltaInvariant
        type D2[A, B] = DeltaInvariant[A, B]
        rw(D2.Insert(1, 1), s"""["$pref2.Insert",{"key":1,"value":1}]""")
        rw(D2.Insert(1, 1): D2[Int, Int], s"""["$pref2.Insert",{"key":1,"value":1}]""")
        rw(D2.Remove(1), s"""["$pref2.Remove",{"key":1}]""")
        rw(D2.Remove(1): D2[Int, Int], s"""["$pref2.Remove",{"key":1}]""")
        rw(D2.Clear(), s"""["$pref2.Clear",{}]""")
        rw(D2.Clear(): D2[Int, Int], s"""["$pref2.Clear",{}]""")
      }
      * - {
        val pref2 = "derive.GenericADTs.DeltaHardcoded"
        val D3 = DeltaHardcoded
        type D3[A, B] = DeltaHardcoded[A, B]
        rw(D3.Insert(Seq(1), "1"), s"""["$pref2.Insert",{"key":[1],"value":"1"}]""")
        rw(D3.Insert(Seq(1), "1"): D3[Seq[Int], String], s"""["$pref2.Insert",{"key":[1],"value":"1"}]""")
        rw(D3.Remove(Seq(1)), s"""["$pref2.Remove",{"key":[1]}]""")
        rw(D3.Remove(Seq(1)): D3[Seq[Int], String], s"""["$pref2.Remove",{"key":[1]}]""")
        rw(D3.Clear(), s"""["$pref2.Clear",{}]""")
        rw(D3.Clear(): D3[Seq[Int], String], s"""["$pref2.Clear",{}]""")
      }
    }
    'recursiveDataTypes{
      import Recursive._
      rw(
        IntTree(123, List(IntTree(456, Nil), IntTree(789, Nil))),
        """{"value":123,"children":[{"value":456,"children":[]},{"value":789,"children":[]}]}"""
      )
      rw(
        SingleNode(123, List(SingleNode(456, Nil), SingleNode(789, Nil))),
        """["derive.Recursive.SingleNode",{"value":123,"children":[["derive.Recursive.SingleNode",{"value":456,"children":[]}],["derive.Recursive.SingleNode",{"value":789,"children":[]}]]}]"""
      )
      rw(
        SingleNode(123, List(SingleNode(456, Nil), SingleNode(789, Nil))): SingleTree,
        """["derive.Recursive.SingleNode",{"value":123,"children":[["derive.Recursive.SingleNode",{"value":456,"children":[]}],["derive.Recursive.SingleNode",{"value":789,"children":[]}]]}]"""
      )
      rw(End: LL, """["derive.Recursive.End",{}]""")
      rw(Node(3, End): LL, """["derive.Recursive.Node",{"c":3,"next":["derive.Recursive.End",{}]}]""")
      rw(Node(6, Node(3, End)), """["derive.Recursive.Node",{"c":6,"next":["derive.Recursive.Node",{"c":3,"next":["derive.Recursive.End",{}]}]}]""")

    }
  }


}
