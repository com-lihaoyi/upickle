package upickle
import utest._

import LegacyTestUtil.rw
import upickle.legacy.{Reader => R, Writer => W, ReadWriter => RW}
object LegacyTests extends TestSuite {
  
  val tests = Tests {
    'sealedHierarchy {
      // objects in sealed case class hierarchies should always read and write
      // the same way (with a tag) regardless of what their static type is when
      // written. This is feasible because sealed hierarchies can only have a
      // finite number of cases, so we can just check them all and decide which
      // class the instance belongs to.
      import Hierarchy._
      implicit def Arw: RW[A] = upickle.legacy.macroRW
      implicit def Brw: RW[B] = upickle.legacy.macroRW
      implicit def Crw: RW[C] = upickle.legacy.macroRW
      'shallow {
        * - rw(B(1), """["upickle.Hierarchy.B",{"i":1}]""")
        * - rw(C("a", "b"), """["upickle.Hierarchy.C",{"s1":"a","s2":"b"}]""")
        //Doesn't work in 2.10.4
        //          * - rw(AnZ: Z, """["upickle.Hierarchy.AnZ",{}]""")
        //          * - rw(AnZ, """["upickle.Hierarchy.AnZ",{}]""")

        * - rw(Hierarchy.B(1): Hierarchy.A, """["upickle.Hierarchy.B",{"i":1}]""")
        * - rw(C("a", "b"): A, """["upickle.Hierarchy.C",{"s1":"a","s2":"b"}]""")
      }
      'deep{
        import DeepHierarchy._
        implicit def Arw: RW[A] = upickle.legacy.macroRW
        implicit def Brw: RW[B] = upickle.legacy.macroRW
        implicit def Crw: RW[C] = upickle.legacy.macroRW
        implicit def AnQrw: RW[AnQ] = upickle.legacy.macroRW
        implicit def Qrw: RW[Q] = upickle.legacy.macroRW
        implicit def Drw: RW[D] = upickle.legacy.macroRW
        implicit def Erw: RW[E] = upickle.legacy.macroRW
        implicit def Frw: RW[F] = upickle.legacy.macroRW
        * - rw(B(1), """["upickle.DeepHierarchy.B",{"i":1}]""")
        * - rw(B(1): A, """["upickle.DeepHierarchy.B",{"i":1}]""")
        * - rw(AnQ(1): Q, """["upickle.DeepHierarchy.AnQ",{"i":1}]""")
        * - rw(AnQ(1), """["upickle.DeepHierarchy.AnQ",{"i":1}]""")

        * - rw(F(AnQ(1)), """["upickle.DeepHierarchy.F",{"q":["upickle.DeepHierarchy.AnQ",{"i":1}]}]""")
        * - rw(F(AnQ(2)): A, """["upickle.DeepHierarchy.F",{"q":["upickle.DeepHierarchy.AnQ",{"i":2}]}]""")
        * - rw(F(AnQ(3)): C, """["upickle.DeepHierarchy.F",{"q":["upickle.DeepHierarchy.AnQ",{"i":3}]}]""")
        * - rw(D("1"), """["upickle.DeepHierarchy.D",{"s":"1"}]""")
        * - rw(D("1"): C, """["upickle.DeepHierarchy.D",{"s":"1"}]""")
        * - rw(D("1"): A, """["upickle.DeepHierarchy.D",{"s":"1"}]""")
        * - rw(E(true), """["upickle.DeepHierarchy.E",{"b":true}]""")
        * - rw(E(true): C, """["upickle.DeepHierarchy.E",{"b":true}]""")
        * - rw(E(true): A, """["upickle.DeepHierarchy.E",{"b":true}]""")
      }
    }
    'singleton {
      import Singletons._
      implicit def AArw: RW[AA] = upickle.legacy.macroRW
      rw(BB, """["upickle.Singletons.BB",{}]""")
      rw(CC, """["upickle.Singletons.CC",{}]""")
      rw(BB: AA, """["upickle.Singletons.BB",{}]""")
      rw(CC: AA, """["upickle.Singletons.CC",{}]""")
    }
    'ADT{
      import GenericADTs._
      * - {
        val pref1 = "upickle.GenericADTs.Delta"
        val D1 = Delta
        implicit def D1rw[A: R: W, B: R: W]: RW[D1[A, B]] = upickle.legacy.macroRW
        implicit def Insertrw[A: R: W, B: R: W]: RW[D1.Insert[A, B]] = upickle.legacy.macroRW
        implicit def Removerw[A: R: W]: RW[D1.Remove[A]] = upickle.legacy.macroRW
        implicit def Clearrw: RW[D1.Clear] = upickle.legacy.macroRW
        type D1[+A, +B] = Delta[A, B]
        rw(D1.Insert(1, 1), s"""["$pref1.Insert",{"key":1,"value":1}]""")
        rw(D1.Insert(1, 1): D1[Int, Int], s"""["$pref1.Insert",{"key":1,"value":1}]""")
        rw(D1.Remove(1), s"""["$pref1.Remove",{"key":1}]""")
        rw(D1.Remove(1): D1[Int, Int], s"""["$pref1.Remove",{"key":1}]""")
        rw(D1.Clear(), s"""["$pref1.Clear",{}]""")
        rw(D1.Clear(): D1[Int, Int], s"""["$pref1.Clear",{}]""")
      }
      * - {
        val pref2 = "upickle.GenericADTs.DeltaInvariant"
        val D2 = DeltaInvariant
        type D2[A, B] = DeltaInvariant[A, B]
        implicit def D2rw[A: R: W, B: R: W]: RW[D2[A, B]] = upickle.legacy.macroRW
        implicit def Insertrw[A: R: W, B: R: W]: RW[D2.Insert[A, B]] = upickle.legacy.macroRW
        implicit def Removerw[A: R: W, B]: RW[D2.Remove[A, B]] = upickle.legacy.macroRW
        implicit def Clearrw[A, B]: RW[D2.Clear[A, B]] = upickle.legacy.macroRW
        rw(D2.Insert(1, 1), s"""["$pref2.Insert",{"key":1,"value":1}]""")
        rw(D2.Insert(1, 1): D2[Int, Int], s"""["$pref2.Insert",{"key":1,"value":1}]""")
        rw(D2.Remove(1), s"""["$pref2.Remove",{"key":1}]""")
        rw(D2.Remove(1): D2[Int, Int], s"""["$pref2.Remove",{"key":1}]""")
        rw(D2.Clear(), s"""["$pref2.Clear",{}]""")
        rw(D2.Clear(): D2[Int, Int], s"""["$pref2.Clear",{}]""")
      }
    }
    'recursiveDataTypes{
      import Recursive._

      implicit def IntTreerw: RW[IntTree] = upickle.legacy.macroRW
      implicit def SingleNoderw: RW[SingleNode] = upickle.legacy.macroRW[SingleNode]
      implicit def SingleTreerw: RW[SingleTree] = upickle.legacy.macroRW[SingleTree]

      implicit def Noderw: RW[Node] = upickle.legacy.macroRW
      implicit def LLrw: RW[LL] = upickle.legacy.macroRW
      rw(
        IntTree(123, List(IntTree(456, Nil), IntTree(789, Nil))),
        """{"value":123,"children":[{"value":456,"children":[]},{"value":789,"children":[]}]}"""
      )
      rw(
        SingleNode(123, List(SingleNode(456, Nil), SingleNode(789, Nil))),
        """["upickle.Recursive.SingleNode",{"value":123,"children":[["upickle.Recursive.SingleNode",{"value":456,"children":[]}],["upickle.Recursive.SingleNode",{"value":789,"children":[]}]]}]"""
      )
      rw(
        SingleNode(123, List(SingleNode(456, Nil), SingleNode(789, Nil))): SingleTree,
        """["upickle.Recursive.SingleNode",{"value":123,"children":[["upickle.Recursive.SingleNode",{"value":456,"children":[]}],["upickle.Recursive.SingleNode",{"value":789,"children":[]}]]}]"""
      )
      rw(End: LL, """["upickle.Recursive.End",{}]""")
      rw(Node(3, End): LL, """["upickle.Recursive.Node",{"c":3,"next":["upickle.Recursive.End",{}]}]""")
      rw(Node(6, Node(3, End)), """["upickle.Recursive.Node",{"c":6,"next":["upickle.Recursive.Node",{"c":3,"next":["upickle.Recursive.End",{}]}]}]""")

    }
  }


}
