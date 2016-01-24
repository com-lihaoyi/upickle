package test.pprint



import derive.Issue92
import utest._
import pprint.Config.Defaults._

case class CustomToString(){
  override def toString = "LA LA LA"
}
sealed trait Customs
object Customs{
  case class A(i: Int) extends Customs
  case class B(s: String) extends Customs{
    override def toString = "Beeee"
  }
}
object DerivationTests extends TestSuite{



  val tests = TestSuite{
    'singletons {
      import derive.Singletons._
      Check(Standalone, "Standalone")
      Check(BB, "BB")
      Check(CC, "CC")
      Check(CC: AA, "CC")
    }
    'adts {
      import derive.ADTs._
      Check(
        ADTb(123, "hello world"),
        """ADTb(123, "hello world")"""
      )

      Check(
        Seq(ADTb(123, "hello world"), ADTb(-999, "i am cow")),
        """List(ADTb(123, "hello world"), ADTb(-999, "i am cow"))"""
      )

      Check(ADT0(), "ADT0()")
    }
    'sealedHierarchies {
      import derive.DeepHierarchy._
      Check(
        AnQ(1),
        "AnQ(1)"
      )
      Check(
        AnQ(1): Q,
        "AnQ(1)"
      )
      Check(
        E(false),
        "E(false)"
      )
      Check(
        F(AnQ(1)): A,
        "F(AnQ(1))"
      )
    }
    'varargs {
      import derive.Varargs._
      Check(Sentence("omg", "2", "3"), """Sentence("omg", Array("2", "3"))""")
    }
    'genericADTs {
      import derive.GenericADTs._
      Check(DeltaHardcoded.Remove("omg"), """Remove("omg")""")
      Check(
        Delta.Insert(List("omg", "wtf"), (1, 0.2)),
        """Insert(List("omg", "wtf"), (1, 0.2))"""
      )
      Check(
        DeltaInvariant.Clear[Int, String](),
        """Clear()"""
      )
      Check(
        DeltaInvariant.Clear(),
        """Clear()"""
      )

      Check(
        DeltaHardcoded.Remove(List(1, 2, 3)): DeltaHardcoded[Seq[Int], String],
        """Remove(List(1, 2, 3))"""
      )
      Check(
        Delta.Insert(List("omg", "wtf"), (1, 0.2)): Delta[List[String], (Int, Double)],
        """Insert(List("omg", "wtf"), (1, 0.2))"""
      )
      Check(
        DeltaInvariant.Clear(): DeltaInvariant[Int, String],
        """Clear()"""
      )

    }


    'fallback{
      // make sure we can pprint stuff that looks nothing like a case class
      // by falling back to good old toString
      import derive.Amorphous._
      val a =  new A()
      Check(a, a.toString)
      Check(a: Any, a.toString)
      Check(Seq("lol", 1, 'c'), "List(lol, 1, c)")
      Check(("lol", 1, 'c'): AnyRef, "(lol,1,c)")

      // Even random non-Scala stuff should work
      val x = new java.util.Random()
      Check(x, x.toString)
//      val z = new UUID(0, -1)
//      Check(z, "00000000-0000-0000-ffff-ffffffffffff")
      // Make sure when dealing with composite data structures, we continue
      // to use the static versions as deep as we can go before falling back
      // to toString
      Check(
        derive.Generic.ADT(x, x: java.io.Serializable, "lol", "lol": Any, (1.5, 2.5), (1.5, 2.5): AnyRef),
        s"""ADT($x, $x, "lol", lol, (1.5, 2.5), (1.5,2.5))"""
      )

    }
    'enums{
      val days1 = pprint.tokenize(java.util.concurrent.TimeUnit.DAYS).mkString
      val days2 = pprint.tokenize(scala.concurrent.duration.SECONDS: java.util.concurrent.TimeUnit).mkString
      assert(
        days1 == "DAYS",
        days2 == "SECONDS"
      )
    }
    'issue92{
      val r = new derive.Issue92.Rational {
        override def compare(that: Issue92.Rational): Int = ???
      }
      Check(r : derive.Issue92.Rational, r.toString)
    }
    'test{
      Check(
        derive.C2(List(derive.C1("hello", List("world")))),
        """C2(List(C1("hello", List("world"))))"""
      )
    }
    'customToString{
      Check(new CustomToString(), "LA LA LA")
      Check(new Customs.A(123), "A(123)")
      Check(new Customs.A(123): Customs, "A(123)")
      Check(new Customs.B("123"), "Beeee")
      Check(new Customs.B("123"): Customs, "Beeee")
    }
  }
}


