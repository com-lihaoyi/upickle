package upickle
import utest._
import upickle.TestUtil._

import upickle.default.{read, write, ReadWriter => RW}

case class Trivial(a: Int = 1)
case class KeyedPerson(
                   @upickle.implicits.key("first_name") firstName: String = "N/A",
                   @upickle.implicits.key("last_name") lastName: String)
object KeyedPerson {
  implicit val rw: RW[KeyedPerson] = upickle.default.macroRW
}

object Custom {
  trait ThingBase{
    val i: Int
    val s: String
    override def equals(o: Any) = {
      o.toString == this.toString
    }

    override def toString() = {
      s"Thing($i, $s)"
    }
  }

  class Thing2(val i: Int, val s: String) extends ThingBase

  abstract class ThingBaseCompanion[T <: ThingBase](f: (Int, String) => T){
    implicit val thing2Writer: RW[T] = upickle.default.readwriter[String].bimap[T](
      t => t.i + " " + t.s,
      str => {
        val Array(i, s) = str.toString.split(" ", 2)
        f(i.toInt, s)
      }
    )
  }
  object Thing2 extends ThingBaseCompanion[Thing2](new Thing2(_, _))

  case class Thing3(i: Int, s: String) extends ThingBase

  object Thing3 extends ThingBaseCompanion[Thing3](new Thing3(_, _))
}

//// this can be un-sealed as long as `derivedSubclasses` is defined in the companion
sealed trait TypedFoo
object TypedFoo{
  import upickle.default._
  implicit val readWriter: ReadWriter[TypedFoo] = ReadWriter.merge(
    macroRW[Bar], macroRW[Baz], macroRW[Quz]
  )

  case class Bar(i: Int) extends TypedFoo
  case class Baz(s: String) extends TypedFoo
  case class Quz(b: Boolean) extends TypedFoo
}
// End TypedFoo

sealed trait SpecialChars

object SpecialChars{
  case class `+1`(`+1`: Int = 0) extends SpecialChars
  case class `-1`(`-1`: Int = 0) extends SpecialChars
  implicit def plusonerw: RW[`+1`] = default.macroRW
  implicit def minusonerw: RW[`-1`] = default.macroRW
  implicit def rw: RW[SpecialChars] = default.macroRW
}

object MacroTests extends TestSuite {

  // Doesn't work :(
//  case class A_(objects: Option[C_]); case class C_(nodes: Option[C_])

//  implicitly[Reader[A_]]
//  implicitly[upickle.old.Writer[upickle.MixedIn.Obj.ClsB]]
//  println(write(ADTs.ADTc(1, "lol", (1.1, 1.2))))
//  implicitly[upickle.old.Writer[ADTs.ADTc]]

  val tests = Tests {
    test("mixedIn"){
      import MixedIn._

      test - rw(Obj.ClsB(1), """{"i":1}""")
      test - rw(Obj.ClsA("omg"), """{"s":"omg"}""")
     }
//
//    /*
//    // TODO Currently not supported
//    test("declarationWithinFunction"){
//      sealed trait Base
//      case object Child extends Base
//      case class Wrapper(base: Base)
//      test - upickle.write(Wrapper(Child))
//    }
//

//    */
    test("exponential"){

      // Doesn't even need to execute, as long as it can compile
      val ww1 = implicitly[upickle.default.Writer[Exponential.A1]]
    }


    test("commonCustomStructures"){
      test("simpleAdt"){

        test - rw(ADTs.ADT0(), """{}""")
        test - rw(ADTs.ADTa(1), """{"i":1}""")
        test - rw(ADTs.ADTb(1, "lol"), """{"i":1,"s":"lol"}""")

        test - rw(ADTs.ADTc(1, "lol", (1.1, 1.2)), """{"i":1,"s":"lol","t":[1.1,1.2]}""")
        test - rw(
          ADTs.ADTd(1, "lol", (1.1, 1.2), ADTs.ADTa(1)),
          """{"i":1,"s":"lol","t":[1.1,1.2],"a":{"i":1}}"""
        )

        test - rw(
          ADTs.ADTe(1, "lol", (1.1, 1.2), ADTs.ADTa(1), List(1.2, 2.1, 3.14)),
          """{"i":1,"s":"lol","t":[1.1,1.2],"a":{"i":1},"q":[1.2,2.1,3.14]}"""
        )

        test - rw(
          ADTs.ADTf(1, "lol", (1.1, 1.2), ADTs.ADTa(1), List(1.2, 2.1, 3.14), Some(None)),
          """{"i":1,"s":"lol","t":[1.1,1.2],"a":{"i":1},"q":[1.2,2.1,3.14],"o":[[]]}"""
        )
        val chunks = for (i <- 1 to 18) yield {
          val rhs = if (i % 2 == 1) "1" else "\"1\""
          val lhs = '"' + s"t$i" + '"'
          s"$lhs:$rhs"
        }

        val expected = s"""{${chunks.mkString(",")}}"""
        test - rw(
          ADTs.ADTz(1, "1", 1, "1", 1, "1", 1, "1", 1, "1", 1, "1", 1, "1", 1, "1", 1, "1"),
          expected
        )
      }

      test("sealedHierarchy"){
        // objects in sealed case class hierarchies should always read and write
        // the same way (with a tag) regardless of what their static type is when
        // written. This is feasible because sealed hierarchies can only have a
        // finite number of cases, so we can just check them all and decide which
        // class the instance belongs to.
        import Hierarchy._
        test("shallow"){
          test - rw(B(1), """{"$type": "upickle.Hierarchy.B", "i":1}""")
          test - rw(C("a", "b"), """{"$type": "upickle.Hierarchy.C", "s1":"a","s2":"b"}""")
          test - rw(AnZ: Z, """ "upickle.Hierarchy.AnZ" """, """{"$type": "upickle.Hierarchy.AnZ"}""")
          test - rw(AnZ, """ "upickle.Hierarchy.AnZ" """, """{"$type": "upickle.Hierarchy.AnZ"}""")
          test - rw(Hierarchy.B(1): Hierarchy.A, """{"$type": "upickle.Hierarchy.B", "i":1}""")
          test - rw(C("a", "b"): A, """{"$type": "upickle.Hierarchy.C", "s1":"a","s2":"b"}""")

        }
        test("tagLast"){
          // Make sure the tagged dictionary parser is able to parse cases where
          // the $type-tag appears later in the dict. It does this by a totally
          // different code-path than for tag-first dicts, using an intermediate
          // AST, so make sure that code path works too.
          test - rw(C("a", "b"), """{"s1":"a","s2":"b", "$type": "upickle.Hierarchy.C"}""")
          test - rw(B(1), """{"i":1, "$type": "upickle.Hierarchy.B"}""")
          test - rw(C("a", "b"): A, """{"s1":"a","s2":"b", "$type": "upickle.Hierarchy.C"}""")
        }
        test("deep"){
          import DeepHierarchy._

          test - rw(B(1), """{"$type": "upickle.DeepHierarchy.B", "i":1}""")
          test - rw(B(1): A, """{"$type": "upickle.DeepHierarchy.B", "i":1}""")
          test - rw(AnQ(1): Q, """{"$type": "upickle.DeepHierarchy.AnQ", "i":1}""")
          test - rw(AnQ(1), """{"$type": "upickle.DeepHierarchy.AnQ","i":1}""")

          test - rw(F(AnQ(1)), """{"$type": "upickle.DeepHierarchy.F","q":{"$type":"upickle.DeepHierarchy.AnQ", "i":1}}""")
          test - rw(F(AnQ(2)): A, """{"$type": "upickle.DeepHierarchy.F","q":{"$type":"upickle.DeepHierarchy.AnQ", "i":2}}""")
          test - rw(F(AnQ(3)): C, """{"$type": "upickle.DeepHierarchy.F","q":{"$type":"upickle.DeepHierarchy.AnQ", "i":3}}""")
          test - rw(D("1"), """{"$type": "upickle.DeepHierarchy.D", "s":"1"}""")
          test - rw(D("1"): C, """{"$type": "upickle.DeepHierarchy.D", "s":"1"}""")
          test - rw(D("1"): A, """{"$type": "upickle.DeepHierarchy.D", "s":"1"}""")
          test - rw(E(true), """{"$type": "upickle.DeepHierarchy.E", "b":true}""")
          test - rw(E(true): C, """{"$type": "upickle.DeepHierarchy.E","b":true}""")
          test - rw(E(true): A, """{"$type": "upickle.DeepHierarchy.E", "b":true}""")
        }
      }
      test("singleton"){
        import Singletons._

        rw(BB, """{"$type":"upickle.Singletons.BB"}""")
        rw(CC, """{"$type":"upickle.Singletons.CC"}""")
        rw(BB: AA, """{"$type":"upickle.Singletons.BB"}""")
        rw(CC: AA, """{"$type":"upickle.Singletons.CC"}""")
      }
    }
    test("robustnessAgainstVaryingSchemas"){
      test("renameKeysViaAnnotations"){
        import Annotated._

        test - rw(B(1), """{"$type": "0", "omg":1}""")
        test - rw(C("a", "b"), """{"$type": "1", "lol":"a","wtf":"b"}""")

        test - rw(B(1): A, """{"$type": "0", "omg":1}""")
        test - rw(C("a", "b"): A, """{"$type": "1", "lol":"a","wtf":"b"}""")
      }
      test("useDefaults"){
        // Ignore the values which match the default when writing and
        // substitute in defaults when reading if the key is missing
        import Defaults._
        test - rw(ADTa(), "{}")
        test - rw(ADTa(321), """{"i":321}""")
        test - rw(ADTb(s = "123"), """{"s":"123"}""")
        test - rw(ADTb(i = 234, s = "567"), """{"i":234,"s":"567"}""")
        test - rw(ADTc(s = "123"), """{"s":"123"}""")
        test - rw(ADTc(i = 234, s = "567"), """{"i":234,"s":"567"}""")
        test - rw(ADTc(t = (12.3, 45.6), s = "789"), """{"s":"789","t":[12.3,45.6]}""")
        test - rw(ADTc(t = (12.3, 45.6), s = "789", i = 31337), """{"i":31337,"s":"789","t":[12.3,45.6]}""")
      }
      test("ignoreExtraFieldsWhenDeserializing"){
        import ADTs._
        val r1 = read[ADTa]( """{"i":123, "j":false, "k":"haha"}""")
        assert(r1 == ADTa(123))
        val r2 = read[ADTb]( """{"i":123, "j":false, "k":"haha", "s":"kk", "l":true, "z":[1, 2, 3]}""")
        assert(r2 == ADTb(123, "kk"))
      }
    }

    test("custom"){
      test("clsReaderWriter"){
        rw(new Custom.Thing2(1, "s"), """ "1 s" """)
        rw(new Custom.Thing2(10, "sss"), """ "10 sss" """)
      }
      test("caseClsReaderWriter"){
        rw(new Custom.Thing3(1, "s"), """ "1 s" """)
        rw(new Custom.Thing3(10, "sss"), """ "10 sss" """)
      }
    }
    test("varargs"){
      rw(Varargs.Sentence("a", "b", "c"), """{"a":"a","bs":["b","c"]}""")
      rw(Varargs.Sentence("a"), """{"a":"a","bs":[]}""")
    }
    test("defaultregression"){
      implicit val rw: upickle.default.ReadWriter[Trivial] = upickle.default.macroRW[Trivial]

      upickle.default.read[Trivial]("{\"a\":2}") ==> Trivial(2)
      upickle.default.read[Trivial]("{}") ==> Trivial(1)

    }
    test("defaultkeyregression"){

      val json = """{"last_name": "Snow"}"""
      upickle.default.read[KeyedPerson](json) ==> KeyedPerson("N/A", "Snow")
    }

    test("specialchars"){
      rw(SpecialChars.`+1`(), """{"$type": "upickle.SpecialChars.+1"}""")
      rw(SpecialChars.`+1`(1), """{"$type": "upickle.SpecialChars.+1", "+1": 1}""")
      rw(SpecialChars.`-1`(), """{"$type": "upickle.SpecialChars.-1"}""")
      rw(SpecialChars.`-1`(1), """{"$type": "upickle.SpecialChars.-1", "-1": 1}""")
    }
  }
}

