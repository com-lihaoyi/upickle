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

@upickle.implicits.key("customKey")
sealed trait KeyedADT
object KeyedADT {
  implicit val rw: RW[KeyedADT] = upickle.default.macroRW

  case object Foo extends KeyedADT {
    implicit val rw: RW[Foo.type] = upickle.default.macroRW
  }
  case class Bar(i: Int) extends KeyedADT
  object Bar {
    implicit val rw: RW[Bar] = upickle.default.macroRW
  }
}

@upickle.implicits.key("customKey1")
sealed trait MultiKeyedADT1
@upickle.implicits.key("customKey2")
sealed trait MultiKeyedADT2
case object MultiKeyedObj extends MultiKeyedADT1 with MultiKeyedADT2

@upickle.implicits.key("customKey")
sealed trait SomeMultiKeyedADT1
sealed trait SomeMultiKeyedADT2
case object SomeMultiKeyedObj extends SomeMultiKeyedADT1 with SomeMultiKeyedADT2

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
      t => s"${t.i} ${t.s}",
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
    macroRW[Bar], macroRW[Baz], macroRW[Quz],
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

object GenericIssue545{
  case class Person(id: Int, name: String = "test")

  implicit val personRw: upickle.default.ReadWriter[Person] = upickle.default.macroRW[Person]

  case class ApiResult[T](data: Option[T] = None, @upickle.implicits.key("total_count") totalCount: Int)

  implicit def apiResultRw[T: upickle.default.ReadWriter]: upickle.default.ReadWriter[ApiResult[T]] = upickle.default.macroRW[ApiResult[T]]
}

object UnknownKeys{
  case class Default(id: Int, name: String)

  implicit val defaultRw: upickle.default.ReadWriter[Default] = upickle.default.macroRW[Default]
  implicit val defaultRw2: DisallowPickler.ReadWriter[Default] = DisallowPickler.macroRW[Default]

  @upickle.implicits.allowUnknownKeys(false)
  case class DisAllow(id: Int, name: String)

  implicit val disAllowRw: upickle.default.ReadWriter[DisAllow] = upickle.default.macroRW[DisAllow]
  implicit val disAllowRw2: DisallowPickler.ReadWriter[DisAllow] = DisallowPickler.macroRW[DisAllow]

  @upickle.implicits.allowUnknownKeys(true)
  case class Allow(id: Int, name: String)

  implicit val allowRw: upickle.default.ReadWriter[Allow] = upickle.default.macroRW[Allow]
  implicit val allowRw2: DisallowPickler.ReadWriter[Allow] = DisallowPickler.macroRW[Allow]

  object DisallowPickler extends upickle.AttributeTagged {
    override def allowUnknownKeys = false
  }
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

        test - rw(ADTs.ADT0(), """{}""", upack.Obj())
        test - rw(ADTs.ADTa(1), """{"i":1}""", upack.Obj(upack.Str("i") -> upack.Int32(1)))
        test - rw(
          ADTs.ADTb(1, "lol"),
          """{"i":1,"s":"lol"}""",
          upack.Obj(upack.Str("i") -> upack.Int32(1), upack.Str("s") -> upack.Str("lol"))
        )

        test - rw(
          ADTs.ADTc(1, "lol", (1.1, 1.2)),
          """{"i":1,"s":"lol","t":[1.1,1.2]}""",
          upack.Obj(
            upack.Str("i") -> upack.Int32(1),
            upack.Str("s") -> upack.Str("lol"),
            upack.Str("t") -> upack.Arr(upack.Float64(1.1), upack.Float64(1.2))
          )
        )
        test - rw(
          ADTs.ADTd(1, "lol", (1.1, 1.2), ADTs.ADTa(1)),
          """{"i":1,"s":"lol","t":[1.1,1.2],"a":{"i":1}}""",
          upack.Obj(
            upack.Str("i") -> upack.Int32(1),
            upack.Str("s") -> upack.Str("lol"),
            upack.Str("t") -> upack.Arr(upack.Float64(1.1), upack.Float64(1.2)),
            upack.Str("a") -> upack.Obj(upack.Str("i") -> upack.Int32(1))
          )
        )

        test - rw(
          ADTs.ADTe(1, "lol", (1.1, 1.2), ADTs.ADTa(1), List(1.2, 2.1, 3.14)),
          """{"i":1,"s":"lol","t":[1.1,1.2],"a":{"i":1},"q":[1.2,2.1,3.14]}""",
          upack.Obj(
            upack.Str("i") -> upack.Int32(1),
            upack.Str("s") -> upack.Str("lol"),
            upack.Str("t") -> upack.Arr(upack.Float64(1.1), upack.Float64(1.2)),
            upack.Str("a") -> upack.Obj(upack.Str("i") -> upack.Int32(1)),
            upack.Str("q") -> upack.Arr(
              upack.Float64(1.2),
              upack.Float64(2.1),
              upack.Float64(3.14)
            )
          )
        )

        test - rw(
          ADTs.ADTf(1, "lol", (1.1, 1.2), ADTs.ADTa(1), List(1.2, 2.1, 3.14), Some(None)),
          """{"i":1,"s":"lol","t":[1.1,1.2],"a":{"i":1},"q":[1.2,2.1,3.14],"o":[[]]}""",
          upack.Obj(
            upack.Str("i") -> upack.Int32(1),
            upack.Str("s") -> upack.Str("lol"),
            upack.Str("t") -> upack.Arr(upack.Float64(1.1), upack.Float64(1.2)),
            upack.Str("a") -> upack.Obj(upack.Str("i") -> upack.Int32(1)),
            upack.Str("q") -> upack.Arr(
              upack.Float64(1.2),
              upack.Float64(2.1),
              upack.Float64(3.14)
            ),
            upack.Str("o") -> upack.Arr(upack.Arr())
          )
        )
        val chunks = for (i <- 1 to 18) yield {
          val rhs = if (i % 2 == 1) "1" else "\"1\""
          val lhs = s""""t$i""""
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

        // Make sure the tagged dictionary parser is able to parse cases where
        // the $type-tag appears later in the dict. It does this by a totally
        // different code-path than for tag-first dicts, using an intermediate
        // AST, so make sure that code path works too.
        import Hierarchy._
        test("shallow"){
          test - rw(
            B(1),
            """{"$type": "B", "i":1}""",
            """{"$type": "upickle.Hierarchy.B", "i":1}""",
            """{"i":1, "$type": "upickle.Hierarchy.B"}""",
            upack.Obj(
              upack.Str("$type") -> upack.Str("B"),
              upack.Str("i") -> upack.Int32(1)
            ),
            upack.Obj(
              upack.Str("$type") -> upack.Str("upickle.Hierarchy.B"),
              upack.Str("i") -> upack.Int32(1)
            ),
            upack.Obj(
              upack.Str("i") -> upack.Int32(1),
              upack.Str("$type") -> upack.Str("upickle.Hierarchy.B")
            )
          )
          test - rw(
            C("a", "b"),
            """{"$type": "C", "s1":"a","s2":"b"}""",
            """{"$type": "upickle.Hierarchy.C", "s1":"a","s2":"b"}""",
            """{"s1":"a","s2":"b", "$type": "upickle.Hierarchy.C"}""",
            upack.Obj(
              upack.Str("$type") -> upack.Str("C"),
              upack.Str("s1") -> upack.Str("a"),
              upack.Str("s2") -> upack.Str("b")
            ),
            upack.Obj(
              upack.Str("$type") -> upack.Str("upickle.Hierarchy.C"),
              upack.Str("s1") -> upack.Str("a"),
              upack.Str("s2") -> upack.Str("b")
            ),
            upack.Obj(
              upack.Str("s1") -> upack.Str("a"),
              upack.Str("s2") -> upack.Str("b"),
              upack.Str("$type") -> upack.Str("upickle.Hierarchy.C")
            )
          )
          test - rw(
            AnZ: Z,
            """ "AnZ" """,
            """ "upickle.Hierarchy.AnZ" """,
            """{"$type": "upickle.Hierarchy.AnZ"}""",
            upack.Str("AnZ"),
            upack.Str("upickle.Hierarchy.AnZ"),
            upack.Obj(upack.Str("$type") -> upack.Str("upickle.Hierarchy.AnZ"))
          )
          test - rw(
            AnZ,
            """ "AnZ" """,
            """ "upickle.Hierarchy.AnZ" """,
            """{"$type": "upickle.Hierarchy.AnZ"}""",
            upack.Str("AnZ"),
            upack.Str("upickle.Hierarchy.AnZ"),
            upack.Obj(upack.Str("$type") -> upack.Str("upickle.Hierarchy.AnZ"))
          )
          test - rw(
            Hierarchy.B(1): Hierarchy.A,
            """{"$type": "B", "i":1}""",
            """{"$type": "upickle.Hierarchy.B", "i":1}""",
            """{"i":1, "$type": "upickle.Hierarchy.B"}""",
            upack.Obj(
              upack.Str("$type") -> upack.Str("B"),
              upack.Str("i") -> upack.Int32(1)
            ),
            upack.Obj(
              upack.Str("$type") -> upack.Str("upickle.Hierarchy.B"),
              upack.Str("i") -> upack.Int32(1)
            ),
            upack.Obj(
              upack.Str("i") -> upack.Int32(1),
              upack.Str("$type") -> upack.Str("upickle.Hierarchy.B")
            )
          )
          test - rw(
            C("a", "b"): A,
            """{"$type": "C", "s1":"a","s2":"b"}""",
            """{"$type": "upickle.Hierarchy.C", "s1":"a","s2":"b"}""",
            """{"s1":"a","s2":"b", "$type": "upickle.Hierarchy.C"}""",
            upack.Obj(
              upack.Str("$type") -> upack.Str("C"),
              upack.Str("s1") -> upack.Str("a"),
              upack.Str("s2") -> upack.Str("b")
            ),
            upack.Obj(
              upack.Str("$type") -> upack.Str("upickle.Hierarchy.C"),
              upack.Str("s1") -> upack.Str("a"),
              upack.Str("s2") -> upack.Str("b")
            ),
            upack.Obj(
              upack.Str("s1") -> upack.Str("a"),
              upack.Str("s2") -> upack.Str("b"),
              upack.Str("$type") -> upack.Str("upickle.Hierarchy.C")
            )
          )
        }

        test("deep"){
          import DeepHierarchy._

          test - rw(
            B(1),
            """{"$type": "B", "i":1}""",
            """{"$type": "upickle.DeepHierarchy.B", "i":1}""",
            upack.Obj(
              upack.Str("$type") -> upack.Str("B"),
              upack.Str("i") -> upack.Int32(1)
            ),
            upack.Obj(
              upack.Str("$type") -> upack.Str("upickle.DeepHierarchy.B"),
              upack.Str("i") -> upack.Int32(1)
            )
          )
          test - rw(
            B(1): A,
            """{"$type": "B", "i":1}""",
            """{"$type": "upickle.DeepHierarchy.B", "i":1}""",
            upack.Obj(
              upack.Str("$type") -> upack.Str("B"),
              upack.Str("i") -> upack.Int32(1)
            ),
            upack.Obj(
              upack.Str("$type") -> upack.Str("upickle.DeepHierarchy.B"),
              upack.Str("i") -> upack.Int32(1)
            )
          )
          test - rw(
            AnQ(1): Q,
            """{"$type": "AnQ", "i":1}""",
            """{"$type": "upickle.DeepHierarchy.AnQ", "i":1}""",
            upack.Obj(
              upack.Str("$type") -> upack.Str("AnQ"),
              upack.Str("i") -> upack.Int32(1)
            ),
            upack.Obj(
              upack.Str("$type") -> upack.Str("upickle.DeepHierarchy.AnQ"),
              upack.Str("i") -> upack.Int32(1)
            )
          )
          test - rw(
            AnQ(1),
            """{"$type": "AnQ","i":1}""",
            """{"$type": "upickle.DeepHierarchy.AnQ","i":1}""",
            upack.Obj(
              upack.Str("$type") -> upack.Str("AnQ"),
              upack.Str("i") -> upack.Int32(1)
            ),
            upack.Obj(
              upack.Str("$type") -> upack.Str("upickle.DeepHierarchy.AnQ"),
              upack.Str("i") -> upack.Int32(1)
            )
          )

          test - rw(
            F(AnQ(1)),
            """{"$type": "F","q":{"$type":"AnQ", "i":1}}""",
            """{"$type": "upickle.DeepHierarchy.F","q":{"$type":"upickle.DeepHierarchy.AnQ", "i":1}}""",
            upack.Obj(
              upack.Str("$type") -> upack.Str("F"),
              upack.Str("q") -> upack.Obj(
                upack.Str("$type") -> upack.Str("AnQ"),
                upack.Str("i") -> upack.Int32(1)
              )
            ),
            upack.Obj(
              upack.Str("$type") -> upack.Str("upickle.DeepHierarchy.F"),
              upack.Str("q") -> upack.Obj(
                upack.Str("$type") -> upack.Str("upickle.DeepHierarchy.AnQ"),
                upack.Str("i") -> upack.Int32(1)
              )
            )
          )
          test - rw(
            F(AnQ(2)): A,
            """{"$type": "F","q":{"$type":"AnQ", "i":2}}""",
            """{"$type": "upickle.DeepHierarchy.F","q":{"$type":"upickle.DeepHierarchy.AnQ", "i":2}}""",
            upack.Obj(
              upack.Str("$type") -> upack.Str("F"),
              upack.Str("q") -> upack.Obj(
                upack.Str("$type") -> upack.Str("AnQ"),
                upack.Str("i") -> upack.Int32(2)
              )
            ),
            upack.Obj(
              upack.Str("$type") -> upack.Str("upickle.DeepHierarchy.F"),
              upack.Str("q") -> upack.Obj(
                upack.Str("$type") -> upack.Str("upickle.DeepHierarchy.AnQ"),
                upack.Str("i") -> upack.Int32(2)
              )
            )
          )
          test - rw(
            F(AnQ(3)): C,
            """{"$type": "F","q":{"$type":"AnQ", "i":3}}""",
            """{"$type": "upickle.DeepHierarchy.F","q":{"$type":"upickle.DeepHierarchy.AnQ", "i":3}}""",
            upack.Obj(
              upack.Str("$type") -> upack.Str("F"),
              upack.Str("q") -> upack.Obj(
                upack.Str("$type") -> upack.Str("AnQ"),
                upack.Str("i") -> upack.Int32(3)
              )
            ),
            upack.Obj(
              upack.Str("$type") -> upack.Str("upickle.DeepHierarchy.F"),
              upack.Str("q") -> upack.Obj(
                upack.Str("$type") -> upack.Str("upickle.DeepHierarchy.AnQ"),
                upack.Str("i") -> upack.Int32(3)
              )
            )
          )
          test - rw(
            D("1"),
            """{"$type": "D", "s":"1"}""",
            """{"$type": "upickle.DeepHierarchy.D", "s":"1"}""",
            upack.Obj(
              upack.Str("$type") -> upack.Str("D"),
              upack.Str("s") -> upack.Str("1")
            ),
            upack.Obj(
              upack.Str("$type") -> upack.Str("upickle.DeepHierarchy.D"),
              upack.Str("s") -> upack.Str("1")
            )
          )
          test - rw(
            D("1"): C,
            """{"$type": "D", "s":"1"}""",
            """{"$type": "upickle.DeepHierarchy.D", "s":"1"}""",
            upack.Obj(
              upack.Str("$type") -> upack.Str("D"),
              upack.Str("s") -> upack.Str("1")
            ),
            upack.Obj(
              upack.Str("$type") -> upack.Str("upickle.DeepHierarchy.D"),
              upack.Str("s") -> upack.Str("1")
            )
          )
          test - rw(
            D("1"): A,
            """{"$type": "D", "s":"1"}""",
            """{"$type": "upickle.DeepHierarchy.D", "s":"1"}""",
            upack.Obj(
              upack.Str("$type") -> upack.Str("D"),
              upack.Str("s") -> upack.Str("1")
            ),
            upack.Obj(
              upack.Str("$type") -> upack.Str("upickle.DeepHierarchy.D"),
              upack.Str("s") -> upack.Str("1")
            )
          )
          test - rw(
            E(true),
            """{"$type": "E", "b":true}""",
            """{"$type": "upickle.DeepHierarchy.E", "b":true}""",
            upack.Obj(
              upack.Str("$type") -> upack.Str("E"),
              upack.Str("b") -> upack.True
            ),
            upack.Obj(
              upack.Str("$type") -> upack.Str("upickle.DeepHierarchy.E"),
              upack.Str("b") -> upack.True
            )
          )
          test - rw(
            E(true): C,
            """{"$type": "E","b":true}""",
            """{"$type": "upickle.DeepHierarchy.E","b":true}""",
            upack.Obj(
              upack.Str("$type") -> upack.Str("E"),
              upack.Str("b") -> upack.True
            ),
            upack.Obj(
              upack.Str("$type") -> upack.Str("upickle.DeepHierarchy.E"),
              upack.Str("b") -> upack.True
            )
          )
          test - rw(
            E(true): A,
            """{"$type": "E", "b":true}""",
            """{"$type": "upickle.DeepHierarchy.E", "b":true}""",
            upack.Obj(
              upack.Str("$type") -> upack.Str("E"),
              upack.Str("b") -> upack.True
            ),
            upack.Obj(
              upack.Str("$type") -> upack.Str("upickle.DeepHierarchy.E"),
              upack.Str("b") -> upack.True
            )
          )
        }
      }
      test("singleton"){
        import Singletons._

        rw(
          BB,
          """ "BB" """,
          """ "upickle.Singletons.BB" """,
          """{"$type":"upickle.Singletons.BB"}""",
          upack.Str("BB"),
          upack.Str("upickle.Singletons.BB"),
          upack.Obj(upack.Str("$type") -> upack.Str("upickle.Singletons.BB"))
        )
        rw(
          CC,
          """ "CC" """,
          """ "upickle.Singletons.CC" """,
          """{"$type":"upickle.Singletons.CC"}""",
          upack.Str("CC"),
          upack.Str("upickle.Singletons.CC"),
          upack.Obj(upack.Str("$type") -> upack.Str("upickle.Singletons.CC"))
        )
        rw(
          BB: AA,
          """ "BB" """,
          """ "upickle.Singletons.BB" """,
          """{"$type":"upickle.Singletons.BB"}""",
          upack.Str("BB"),
          upack.Str("upickle.Singletons.BB"),
          upack.Obj(upack.Str("$type") -> upack.Str("upickle.Singletons.BB"))
        )
        rw(
          CC: AA,
          """ "CC" """,
          """ "upickle.Singletons.CC" """,
          """{"$type":"upickle.Singletons.CC"}""",
          upack.Str("CC"),
          upack.Str("upickle.Singletons.CC"),
          upack.Obj(upack.Str("$type") -> upack.Str("upickle.Singletons.CC"))
        )
      }
    }
    test("robustnessAgainstVaryingSchemas"){
      test("renameKeysViaAnnotations"){
        import Annotated._

        test - rw(
          B(1),
          """{"$type": "0", "omg":1}""",
          upack.Obj(
            upack.Str("$type") -> upack.Str("0"),
            upack.Str("omg") -> upack.Int32(1)
          )
        )
        test - rw(
          C("a", "b"),
          """{"$type": "1", "lol":"a","wtf":"b"}""",
          upack.Obj(
            upack.Str("$type") -> upack.Str("1"),
            upack.Str("lol") -> upack.Str("a"),
            upack.Str("wtf") -> upack.Str("b")
          )
        )

        test - rw(
          B(1): A,
          """{"$type": "0", "omg":1}""",
          upack.Obj(
            upack.Str("$type") -> upack.Str("0"),
            upack.Str("omg") -> upack.Int32(1)
          )
        )
        test - rw(
          C("a", "b"): A,
          """{"$type": "1", "lol":"a","wtf":"b"}""",
          upack.Obj(
            upack.Str("$type") -> upack.Str("1"),
            upack.Str("lol") -> upack.Str("a"),
            upack.Str("wtf") -> upack.Str("b")
          )
        )
      }
      test("useDefaults"){
        // Ignore the values which match the default when writing and
        // substitute in defaults when reading if the key is missing
        import Defaults._
        test - rw(ADTa(), "{}", upack.Obj())
        test - rw(
          ADTa(321),
          """{"i":321}""",
          upack.Obj(upack.Str("i") -> upack.Int32(321))
        )
        test - rw(
          ADTb(s = "123"),
          """{"s":"123"}""",
          upack.Obj(upack.Str("s") -> upack.Str("123"))
        )
        test - rw(
          ADTb(i = 234, s = "567"),
          """{"i":234,"s":"567"}""",
          upack.Obj(
            upack.Str("i") -> upack.Int32(234),
            upack.Str("s") -> upack.Str("567")
          )
        )
        test - rw(
          ADTc(s = "123"),
          """{"s":"123"}""",
          upack.Obj(upack.Str("s") -> upack.Str("123"))
        )
        test - rw(
          ADTc(i = 234, s = "567"),
          """{"i":234,"s":"567"}""",
          upack.Obj(
            upack.Str("i") -> upack.Int32(234),
            upack.Str("s") -> upack.Str("567")
          )
        )
        test - rw(
          ADTc(t = (12.3, 45.6), s = "789"),
          """{"s":"789","t":[12.3,45.6]}""",
          upack.Obj(
            upack.Str("s") -> upack.Str("789"),
            upack.Str("t") -> upack.Arr(upack.Float64(12.3), upack.Float64(45.6))
          )
        )
        test - rw(
          ADTc(t = (12.3, 45.6), s = "789", i = 31337),
          """{"i":31337,"s":"789","t":[12.3,45.6]}""",
          upack.Obj(
            upack.Str("i") -> upack.Int32(31337),
            upack.Str("s") -> upack.Str("789"),
            upack.Str("t") -> upack.Arr(upack.Float64(12.3), upack.Float64(45.6))
          )
        )
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
        rw(new Custom.Thing2(1, "s"), """ "1 s" """, upack.Str("1 s"))
        rw(new Custom.Thing2(10, "sss"), """ "10 sss" """, upack.Str("10 sss"))
      }
      test("caseClsReaderWriter"){
        rw(new Custom.Thing3(1, "s"), """ "1 s" """, upack.Str("1 s"))
        rw(new Custom.Thing3(10, "sss"), """ "10 sss" """, upack.Str("10 sss"))
      }
    }
    test("varargs"){
      rw(
        Varargs.Sentence("a", "b", "c"),
         """{"a":"a","bs":["b","c"]}""",
        upack.Obj(
          upack.Str("a") -> upack.Str("a"),
          upack.Str("bs") -> upack.Arr(upack.Str("b"), upack.Str("c"))
        )
      )
      rw(
        Varargs.Sentence("a"),
        """{"a":"a","bs":[]}""",
        upack.Obj(upack.Str("a") -> upack.Str("a"), upack.Str("bs") -> upack.Arr())
      )
    }
    test("defaultregression"){
      implicit val rw: upickle.default.ReadWriter[Trivial] = upickle.default.macroRW[Trivial]

      upickle.default.read[Trivial]("{\"a\":2}") ==> Trivial(2)
      upickle.default.read[Trivial]("{}") ==> Trivial(1)

    }
    test("defaultkeyregression"){
      val json = """{"last_name":"Snow"}"""
      upickle.default.read[KeyedPerson](json) ==> KeyedPerson("N/A", "Snow")
      upickle.default.write[KeyedPerson](KeyedPerson("N/A", "Snow")) ==> json
    }

    test("specialchars"){
      rw(
        SpecialChars.`+1`(),
         """{"$type": "+1"}""",
         """{"$type": "upickle.SpecialChars.+1"}""",
      )
      rw(
        SpecialChars.`+1`(1),
        """{"$type": "+1", "+1": 1}""",
        """{"$type": "upickle.SpecialChars.+1", "+1": 1}"""
      )
      rw(
        SpecialChars.`-1`(),
        """{"$type": "-1"}""",
        """{"$type": "upickle.SpecialChars.-1"}""",
      )
      rw(
        SpecialChars.`-1`(1),
         """{"$type": "-1", "-1": 1}""",
         """{"$type": "upickle.SpecialChars.-1", "-1": 1}""",
      )
    }

    test("genericIssue545"){
      // Make sure case class default values are properly picked up for
      // generic case classes in Scala 3
      upickle.default.read[GenericIssue545.Person]("{\"id\":1}") ==>
        GenericIssue545.Person(1)

      upickle.default.read[GenericIssue545.ApiResult[GenericIssue545.Person]]("{\"total_count\": 10}") ==>
        GenericIssue545.ApiResult[GenericIssue545.Person](None, 10)
    }

    test("unknownKeys"){
      // For upickle default, we defualt to allowing unknown keys, and explicitly annotating
      // `@allowUnknownKeys(true)` does nothing, but `@allowUnknownKeys(false)` makes unknown
      // keys an error (just for the annotated class)
      upickle.default.read[UnknownKeys.Default]("""{"id":1, "name":"x", "omg": "wtf"}""") ==>
        UnknownKeys.Default(1, "x")

      upickle.default.read[UnknownKeys.Allow]("""{"id":1, "name":"x", "omg": "wtf"}""") ==>
        UnknownKeys.Allow(1, "x")

      intercept[upickle.core.AbortException]{
        upickle.default.read[UnknownKeys.DisAllow]("""{"id":1, "name":"x", "omg": "wtf"}""")
      }

      // If the upickle API sets `override def allowUnknownKeys = false`, we default to treating unknown keys
      // as an error, `@allowUnknownKeys(false)` does nothing, but `@allowUnknownKeys(true)` makes unknown
      // keys get ignored (just for the annotated class)
      intercept[upickle.core.AbortException] {
        UnknownKeys.DisallowPickler.read[UnknownKeys.Default]("""{"id":1, "name":"x", "omg": "wtf"}""") ==>
          UnknownKeys.Default(1, "x")
      }

      UnknownKeys.DisallowPickler.read[UnknownKeys.Allow]("""{"id":1, "name":"x", "omg": "wtf"}""") ==>
        UnknownKeys.Allow(1, "x")

      intercept[upickle.core.AbortException]{
        UnknownKeys.DisallowPickler.read[UnknownKeys.DisAllow]("""{"id":1, "name":"x", "omg": "wtf"}""")
      }
    }

    test("keyedADT") {
      val fooJson = "\"Foo\""
      upickle.default.read[KeyedADT](fooJson) ==> KeyedADT.Foo
      upickle.default.write[KeyedADT](KeyedADT.Foo) ==> fooJson

      val barJson = """{"customKey":"Bar","i":1}"""
      upickle.default.read[KeyedADT](barJson) ==> KeyedADT.Bar(1)
      upickle.default.write[KeyedADT](KeyedADT.Bar(1)) ==> barJson
    }

    test("multiKeyedADT") {
      compileError("upickle.default.macroRW[upickle.MultiKeyedObj.type]")
        .check("", "inherits from multiple parent types with different discriminator keys")

      compileError("upickle.default.macroRW[upickle.SomeMultiKeyedObj.type]")
        .check("", "inherits from multiple parent types with different discriminator keys")
    }

    test("multiKeyedADT") {
      compileError("upickle.default.macroRW[upickle.MultiKeyedObj.type]")
        .check("", "inherits from multiple parent types with different discriminator keys")

      compileError("upickle.default.macroRW[upickle.SomeMultiKeyedObj.type]")
        .check("", "inherits from multiple parent types with different discriminator keys")
    }

    test("objectTypeKeyWriteFullyQualified") {
      object customPickler extends upickle.AttributeTagged {
        override def objectTypeKeyWriteFullyQualified = true
      }

      val testutil = new TestUtil(customPickler)

      implicit def rwA: customPickler.ReadWriter[upickle.Hierarchy.A] = customPickler.macroRW
      implicit def rwB: customPickler.ReadWriter[upickle.Hierarchy.B] = customPickler.macroRW
      implicit def rwC: customPickler.ReadWriter[upickle.Hierarchy.C] = customPickler.macroRW

      // Make sure both custom pickler and default pickler can read both long and short `$type` tags,
      // but that the custom pickler generates the long `$type` tag while the default pickler
      // generates the short one
      testutil.rw(new Hierarchy.B(1), """{"$type": "upickle.Hierarchy.B"}""", """{"$type": "B"}""")
      rw(new Hierarchy.B(1), """{"$type": "B"}""", """{"$type": "upickle.Hierarchy.B"}""")

      testutil.rw(new Hierarchy.C("x", "y"), """{"$type": "upickle.Hierarchy.C"}""", """{"$type": "C"}""")
      rw(new Hierarchy.C("x", "y"), """{"$type": "C"}""", """{"$type": "upickle.Hierarchy.C"}""")

    }
  }
}
