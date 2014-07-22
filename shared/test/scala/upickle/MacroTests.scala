package upickle

import utest._
import upickle.TestUtil._
import scala.Some
// These guys all have to be out here because uPickle doesn't
// support pickling local classes and objects
object ADTs {
  case class ADT0()
  case class ADTa(i: Int)
  case class ADTb(i: Int, s: String)
  case class ADTc(i: Int, s: String, t: (Double, Double))
  case class ADTd(i: Int, s: String, t: (Double, Double), a: ADTa)
  case class ADTe(i: Int, s: String, t: (Double, Double), a: ADTa, q: Seq[Double])
  case class ADTf(i: Int, s: String, t: (Double, Double), a: ADTa, q: Seq[Double], o: Option[Option[Boolean]])
  case class ADTz(t1: Int, t2: String,
                  t3: Int, t4: String,
                  t5: Int, t6: String,
                  t7: Int, t8: String,
                  t9: Int, t10: String,
                  t11: Int, t12: String,
                  t13: Int, t14: String,
                  t15: Int, t16: String,
                  t17: Int, t18: String
                   )
}
object Hierarchy {
  sealed trait A
  case class B(i: Int) extends A
  case class C(s1: String, s2: String) extends A
}
object DeepHierarchy {
  sealed trait A
  case class B(i: Int) extends A
  sealed trait C extends A
  case class D(s: String) extends C
  case class E(b: Boolean) extends C
}
object Singletons{
  sealed trait AA
  case object BB extends AA
  case object CC extends AA
}
object Generic{
  case class A[T](t: T)
  case class ADT[A, B, C, D, E, F](a: A, b: B, c: C, d: D, e: E, f: F)
}
object Recursive{
  sealed trait LL
  case object End  extends LL
  case class Node(c: Int, next: LL) extends LL
}
object Annotated {
  sealed trait A
  @key("0") case class B(@key("omg") i: Int) extends A
  @key("1") case class C(@key("lol") s1: String, @key("wtf") s2: String) extends A
}
object Defaults {
  case class ADTa(i: Int = 0)
  case class ADTb(i: Int = 1, s: String)
  case class ADTc(i: Int = 2, s: String, t: (Double, Double) = (1, 2))
}
object MacroTests extends TestSuite{
  val tests = TestSuite{
    'commonCustomStructures{
      'simpleAdt {

        rw(ADTs.ADT0(), """{}""")
        rw(ADTs.ADTa(1), """{"i": 1}""")
        rw(ADTs.ADTb(1, "lol"), """{"i": 1, "s": "lol"}""")

        rw(ADTs.ADTc(1, "lol", (1.1, 1.2)), """{"i": 1, "s": "lol", "t": [1.1, 1.2]}""")
        rw(
          ADTs.ADTd(1, "lol", (1.1, 1.2), ADTs.ADTa(1)),
          """{"i": 1, "s": "lol", "t": [1.1, 1.2], "a": {"i": 1}}"""
        )

        rw(
          ADTs.ADTe(1, "lol", (1.1, 1.2), ADTs.ADTa(1), List(1.2, 2.1, 3.14)),
          """{"i": 1, "s": "lol", "t": [1.1, 1.2], "a": {"i": 1}, "q": [1.2, 2.1, 3.14]}"""
        )
        rw(
          ADTs.ADTf(1, "lol", (1.1, 1.2), ADTs.ADTa(1), List(1.2, 2.1, 3.14), Some(None)),
          """{"i": 1, "s": "lol", "t": [1.1, 1.2], "a": {"i": 1}, "q": [1.2, 2.1, 3.14], "o": [[]]}"""
        )
        val chunks = for (i <- 1 to 18) yield {
          val rhs = if (i % 2 == 1) "1" else "\"1\""
          val lhs = '"' + s"t$i" + '"'
          s"$lhs: $rhs"
        }

        val expected = s"""{${chunks.mkString(", ")}}"""

        rw(
          ADTs.ADTz(1, "1", 1, "1", 1, "1", 1, "1", 1, "1", 1, "1", 1, "1", 1, "1", 1, "1"),
          expected
        )
      }
      'sealedHierarchy {
        // objects in sealed case class hierarchies should always read and write
        // the same way (with a tag) regardless of what their static type is when
        // written. This is feasible because sealed hierarchies can only have a
        // finite number of cases, so we can just check them all and decide which
        // class the instance belongs to.
        import Hierarchy._
        'shallow {
          rw(B(1), """["upickle.Hierarchy.B", {"i": 1}]""")
          rw(C("a", "b"), """["upickle.Hierarchy.C", {"s1": "a", "s2": "b"}]""")

          rw(Hierarchy.B(1): Hierarchy.A, """["upickle.Hierarchy.B", {"i": 1}]""")
          rw(C("a", "b"): A, """["upickle.Hierarchy.C", {"s1": "a", "s2": "b"}]""")
        }
        'deep{
          import DeepHierarchy._

          rw(B(1), """["upickle.DeepHierarchy.B", {"i": 1}]""")
          rw(B(1): A, """["upickle.DeepHierarchy.B", {"i": 1}]""")
          rw(D("1"), """["upickle.DeepHierarchy.D", {"s": "1"}]""")
          rw(D("1"): C, """["upickle.DeepHierarchy.D", {"s": "1"}]""")
          rw(D("1"): A, """["upickle.DeepHierarchy.D", {"s": "1"}]""")
          rw(E(true), """["upickle.DeepHierarchy.E", {"b": true}]""")
          rw(E(true): C, """["upickle.DeepHierarchy.E", {"b": true}]""")
          rw(E(true): A, """["upickle.DeepHierarchy.E", {"b": true}]""")
        }
      }
      'singleton {
        import Singletons._

        //        rw(BB, """[0, []]""")
        //        rw(BC, """[1, []]""")
        rw(BB: AA, """["upickle.Singletons.BB", {}]""")
        rw(CC: AA, """["upickle.Singletons.CC", {}]""")
      }
    }
    'robustnessAgainstVaryingSchemas {
      'renameKeysViaAnnotations {
        import Annotated._

        rw(B(1), """["0", {"omg": 1}]""")
        rw(C("a", "b"), """["1", {"lol": "a", "wtf": "b"}]""")

        rw(B(1): A, """["0", {"omg": 1}]""")
        rw(C("a", "b"): A, """["1", {"lol": "a", "wtf": "b"}]""")
      }
      'useDefaults {
        // Ignore the values which match the default when writing and
        // substitute in defaults when reading if the key is missing
        import Defaults._
        rw(ADTa(), "{}")
        rw(ADTa(321), """{"i": 321}""")
        rw(ADTb(s = "123"), """{"s": "123"}""")
        rw(ADTb(i = 234, s = "567"), """{"i": 234, "s": "567"}""")
        rw(ADTc(s = "123"), """{"s": "123"}""")
        rw(ADTc(i = 234, s = "567"), """{"i": 234, "s": "567"}""")
        rw(ADTc(t = (12.3, 45.6), s = "789"), """{"s": "789", "t": [12.3, 45.6]}""")
        rw(ADTc(t = (12.3, 45.6), s = "789", i = 31337), """{"i": 31337, "s": "789", "t": [12.3, 45.6]}""")
      }
      'ignoreExtraFieldsWhenDeserializing {
        import ADTs._
        val r1 = read[ADTa]( """{i: 123, j: false, k: "haha"}""")
        assert(r1 == ADTa(123))
        val r2 = read[ADTb]( """{i: 123, j: false, k: "haha", s: "kk", l: true, z: [1, 2, 3]}""")
        assert(r2 == ADTb(123, "kk"))
      }
    }
    'GenericDataTypes{
      'simple {
        import Generic.A
        rw(A(1), """{"t": 1}""")
        rw(A("1"), """{"t": "1"}""")
        rw(A(Seq("1", "2", "3")), """{"t": ["1", "2", "3"]}""")
        rw(A(A(A(A(A(A(A(1))))))), """{"t": {"t": {"t": {"t": {"t": {"t": {"t": 1}}}}}}}""")
      }
      'large{
        import Generic.ADT
        rw(ADT(1, 2, 3, 4, 5, 6), """{"a": 1, "b": 2, "c": 3, "d": 4, "e": 5, "f": 6}""")
        rw(
          ADT(
            ADT(1, 2, 3, 4, 5, 6),
            ADT(1, 2, 3, 4, 5, 6),
            ADT(1, 2, 3, 4, 5, 6),
            ADT(1, 2, 3, 4, 5, 6),
            ADT(1, 2, 3, 4, 5, 6),
            ADT(1, 2, 3, 4, 5, 6)
          ),
          """{"a": {"a": 1, "b": 2, "c": 3, "d": 4, "e": 5, "f": 6}, "b": {"a": 1, "b": 2, "c": 3, "d": 4, "e": 5, "f": 6}, "c": {"a": 1, "b": 2, "c": 3, "d": 4, "e": 5, "f": 6}, "d": {"a": 1, "b": 2, "c": 3, "d": 4, "e": 5, "f": 6}, "e": {"a": 1, "b": 2, "c": 3, "d": 4, "e": 5, "f": 6}, "f": {"a": 1, "b": 2, "c": 3, "d": 4, "e": 5, "f": 6}}"""
        )
      }
    }

    'recursiveDataTypes{
      import Recursive._

      rw(End: LL, """["upickle.Recursive.End", {}]""")
      rw(Node(3, End): LL, """["upickle.Recursive.Node", {"c": 3, "next": ["upickle.Recursive.End", {}]}]""")
      rw(Node(6, Node(3, End)), """["upickle.Recursive.Node", {"c": 6, "next": ["upickle.Recursive.Node", {"c": 3, "next": ["upickle.Recursive.End", {}]}]}]""")
    }
    'performance{
      import Generic.ADT
      import Hierarchy._
      import Recursive._
      import Defaults._
      import ADTs.ADT0
      def readData = (s: String) => read[ADT[Seq[(Int, Int)], String, A, LL, ADTc, ADT0]](s)
      // Some arbitrary data that represents a mix of all the different
      // ways things can be pickled and unpickled
      val data = ADT(
        Vector((1, 2), (3, 4), (4, 5), (6, 7), (8, 9), (10, 11), (12, 13)),
        """
          |I am cow, hear me moo
          |I weigh twice as much as you
          |And I look good on the barbecueeeee
        """.stripMargin,
        C("lol i am a noob", "haha you are a noob"): A,
        Node(-11, Node(-22, Node(-33, Node(-44, End)))): LL,
        ADTc(i = 1234567890, s = "i am a strange loop"),
        ADT0()
      )
      val stringified = write(data)
      assert(data == readData(stringified))
      val rewritten = write(readData(stringified))
      assert(stringified == rewritten)

      'read{
        var n = 0
        val start = System.currentTimeMillis()
        while(System.currentTimeMillis() < start + 5000){
          readData(stringified)
          n += 1
        }
        n
      }
      'write{
        var n = 0
        val start = System.currentTimeMillis()
        while(System.currentTimeMillis() < start + 5000){
          write(data)
          n += 1
        }
        n
      }
    }
  }
}
