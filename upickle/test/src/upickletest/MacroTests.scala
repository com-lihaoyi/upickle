package upickletest
import utest._
import upickletest.TestUtil._

import upickle.default.{read, write, ReadWriter => RW, macroRW}

case class Trivial(a: Int = 1)

sealed case class SealedClass(i: Int, s: String)
object SealedClass {
  implicit val rw: RW[SealedClass] = upickle.default.macroRW
}

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
  implicit def plusonerw: RW[`+1`] =macroRW
  implicit def minusonerw: RW[`-1`] =macroRW
  implicit def rw: RW[SpecialChars] =macroRW
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

object TagName{
  object TagNamePickler extends upickle.AttributeTagged {
    override def tagName = "_tag"
  }

  sealed trait Foo
  case class Bar(x: Int) extends Foo
  case class Qux(s: String) extends Foo

  implicit val barRw: TagNamePickler.ReadWriter[Bar] = TagNamePickler.macroRW
  implicit val quxRw: TagNamePickler.ReadWriter[Qux] = TagNamePickler.macroRW
  implicit val fooRw: TagNamePickler.ReadWriter[Foo] = TagNamePickler.macroRW
}

object Flatten {
  case class FlattenTest(i: Int, s: String, @upickle.implicits.flatten n: Nested, @upickle.implicits.flatten n2: Nested2)

  object FlattenTest {
    implicit val rw: RW[FlattenTest] = upickle.default.macroRW
  }

  case class Nested(d: Double, @upickle.implicits.flatten m: Map[String, Int])

  object Nested {
    implicit val rw: RW[Nested] = upickle.default.macroRW
  }

  case class Nested2(name: String)

  object Nested2 {
    implicit val rw: RW[Nested2] = upickle.default.macroRW
  }

  case class FlattenTestWithType[T](i: Int, @upickle.implicits.flatten t: T)

  object FlattenTestWithType {
    implicit val rw: RW[FlattenTestWithType[Nested]] = upickle.default.macroRW
  }

  case class InnerMost(a: String, b: Int)

  object InnerMost {
    implicit val rw: RW[InnerMost] = upickle.default.macroRW
  }

  case class Inner(@upickle.implicits.flatten innerMost: InnerMost, c: Boolean)

  object Inner {
    implicit val rw: RW[Inner] = upickle.default.macroRW
  }

  case class Outer(d: Double, @upickle.implicits.flatten inner: Inner)

  object Outer {
    implicit val rw: RW[Outer] = upickle.default.macroRW
  }

  case class HasMap(@upickle.implicits.flatten map: Map[String, String], i: Int)
  object HasMap {
    implicit val rw: RW[HasMap] = upickle.default.macroRW
  }

  case class FlattenWithDefault(i: Int, @upickle.implicits.flatten n: NestedWithDefault)
  object FlattenWithDefault {
    implicit val rw: RW[FlattenWithDefault] = upickle.default.macroRW
  }
  case class NestedWithDefault(k: Int = 100, l: String)
  object NestedWithDefault {
    implicit val rw: RW[NestedWithDefault] = upickle.default.macroRW
  }

  case class FlattenSeq(@upickle.implicits.flatten n: Seq[(String, Int)])
  object FlattenSeq {
    implicit val rw: RW[FlattenSeq] = upickle.default.macroRW
  }

  case class ValueClass(value: Double)
  object ValueClass {
     implicit val rw: RW[ValueClass] = upickle.default.macroRW
   }
  case class Collection(@upickle.implicits.flatten n: scala.collection.mutable.LinkedHashMap[String, ValueClass])
  object Collection {
    implicit val rw: RW[Collection] = upickle.default.macroRW
  }

  // Test cases for generalized key types (non-String keys)
  case class FlattenIntKey(i: Int, @upickle.implicits.flatten m: Map[Int, String])
  object FlattenIntKey {
    implicit val rw: RW[FlattenIntKey] = upickle.default.macroRW
  }

  case class FlattenSeqIntKey(@upickle.implicits.flatten n: Seq[(Int, String)])
  object FlattenSeqIntKey {
    implicit val rw: RW[FlattenSeqIntKey] = upickle.default.macroRW
  }

  case class FlattenLongKey(@upickle.implicits.flatten m: Map[Long, Int])
  object FlattenLongKey {
    implicit val rw: RW[FlattenLongKey] = upickle.default.macroRW
  }

  // Test case for BufferedValue keys to verify index is preserved
  object BufferedValueKey {
    import upickle.core.{BufferedValue, Visitor}

    private implicit val bufferedR: upickle.default.Reader[BufferedValue] =
      new upickle.default.Reader.Delegate(BufferedValue.Builder)

    private implicit val bufferedW: upickle.default.Writer[BufferedValue] =
      new upickle.default.Writer[BufferedValue] {
        def write0[V](out: Visitor[_, V], v: BufferedValue): V =
          BufferedValue.transform(v, out)
      }

    case class FlattenBufferedKey(@upickle.implicits.flatten m: Map[BufferedValue, String])
    object FlattenBufferedKey {
      implicit val rw: RW[FlattenBufferedKey] = upickle.default.macroRW
    }
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
          """{"i":1,"s":"lol","t":[1.1,1.2],"a":{"i":1},"q":[1.2,2.1,3.14],"o":[null]}""",
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
            upack.Str("o") -> upack.Arr(upack.Null)
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
            """{"$type": "upickletest.Hierarchy.B", "i":1}""",
            """{"i":1, "$type": "upickletest.Hierarchy.B"}""",
            upack.Obj(
              upack.Str("$type") -> upack.Str("B"),
              upack.Str("i") -> upack.Int32(1)
            ),
            upack.Obj(
              upack.Str("$type") -> upack.Str("upickletest.Hierarchy.B"),
              upack.Str("i") -> upack.Int32(1)
            ),
            upack.Obj(
              upack.Str("i") -> upack.Int32(1),
              upack.Str("$type") -> upack.Str("upickletest.Hierarchy.B")
            )
          )
          test - rw(
            C("a", "b"),
            """{"$type": "C", "s1":"a","s2":"b"}""",
            """{"$type": "upickletest.Hierarchy.C", "s1":"a","s2":"b"}""",
            """{"s1":"a","s2":"b", "$type": "upickletest.Hierarchy.C"}""",
            upack.Obj(
              upack.Str("$type") -> upack.Str("C"),
              upack.Str("s1") -> upack.Str("a"),
              upack.Str("s2") -> upack.Str("b")
            ),
            upack.Obj(
              upack.Str("$type") -> upack.Str("upickletest.Hierarchy.C"),
              upack.Str("s1") -> upack.Str("a"),
              upack.Str("s2") -> upack.Str("b")
            ),
            upack.Obj(
              upack.Str("s1") -> upack.Str("a"),
              upack.Str("s2") -> upack.Str("b"),
              upack.Str("$type") -> upack.Str("upickletest.Hierarchy.C")
            )
          )
          test - rw(
            AnZ: Z,
            """ "AnZ" """,
            """ "upickletest.Hierarchy.AnZ" """,
            """{"$type": "upickletest.Hierarchy.AnZ"}""",
            upack.Str("AnZ"),
            upack.Str("upickletest.Hierarchy.AnZ"),
            upack.Obj(upack.Str("$type") -> upack.Str("upickletest.Hierarchy.AnZ"))
          )
          test - rw(
            AnZ,
            """ "AnZ" """,
            """ "upickletest.Hierarchy.AnZ" """,
            """{"$type": "upickletest.Hierarchy.AnZ"}""",
            upack.Str("AnZ"),
            upack.Str("upickletest.Hierarchy.AnZ"),
            upack.Obj(upack.Str("$type") -> upack.Str("upickletest.Hierarchy.AnZ"))
          )
          test - rw(
            Hierarchy.B(1): Hierarchy.A,
            """{"$type": "B", "i":1}""",
            """{"$type": "upickletest.Hierarchy.B", "i":1}""",
            """{"i":1, "$type": "upickletest.Hierarchy.B"}""",
            upack.Obj(
              upack.Str("$type") -> upack.Str("B"),
              upack.Str("i") -> upack.Int32(1)
            ),
            upack.Obj(
              upack.Str("$type") -> upack.Str("upickletest.Hierarchy.B"),
              upack.Str("i") -> upack.Int32(1)
            ),
            upack.Obj(
              upack.Str("i") -> upack.Int32(1),
              upack.Str("$type") -> upack.Str("upickletest.Hierarchy.B")
            )
          )
          test - rw(
            C("a", "b"): A,
            """{"$type": "C", "s1":"a","s2":"b"}""",
            """{"$type": "upickletest.Hierarchy.C", "s1":"a","s2":"b"}""",
            """{"s1":"a","s2":"b", "$type": "upickletest.Hierarchy.C"}""",
            upack.Obj(
              upack.Str("$type") -> upack.Str("C"),
              upack.Str("s1") -> upack.Str("a"),
              upack.Str("s2") -> upack.Str("b")
            ),
            upack.Obj(
              upack.Str("$type") -> upack.Str("upickletest.Hierarchy.C"),
              upack.Str("s1") -> upack.Str("a"),
              upack.Str("s2") -> upack.Str("b")
            ),
            upack.Obj(
              upack.Str("s1") -> upack.Str("a"),
              upack.Str("s2") -> upack.Str("b"),
              upack.Str("$type") -> upack.Str("upickletest.Hierarchy.C")
            )
          )
        }

        test("deep"){
          import DeepHierarchy._

          test - rw(
            B(1),
            """{"$type": "B", "i":1}""",
            """{"$type": "upickletest.DeepHierarchy.B", "i":1}""",
            upack.Obj(
              upack.Str("$type") -> upack.Str("B"),
              upack.Str("i") -> upack.Int32(1)
            ),
            upack.Obj(
              upack.Str("$type") -> upack.Str("upickletest.DeepHierarchy.B"),
              upack.Str("i") -> upack.Int32(1)
            )
          )
          test - rw(
            B(1): A,
            """{"$type": "B", "i":1}""",
            """{"$type": "upickletest.DeepHierarchy.B", "i":1}""",
            upack.Obj(
              upack.Str("$type") -> upack.Str("B"),
              upack.Str("i") -> upack.Int32(1)
            ),
            upack.Obj(
              upack.Str("$type") -> upack.Str("upickletest.DeepHierarchy.B"),
              upack.Str("i") -> upack.Int32(1)
            )
          )
          test - rw(
            AnQ(1): Q,
            """{"$type": "AnQ", "i":1}""",
            """{"$type": "upickletest.DeepHierarchy.AnQ", "i":1}""",
            upack.Obj(
              upack.Str("$type") -> upack.Str("AnQ"),
              upack.Str("i") -> upack.Int32(1)
            ),
            upack.Obj(
              upack.Str("$type") -> upack.Str("upickletest.DeepHierarchy.AnQ"),
              upack.Str("i") -> upack.Int32(1)
            )
          )
          test - rw(
            AnQ(1),
            """{"$type": "AnQ","i":1}""",
            """{"$type": "upickletest.DeepHierarchy.AnQ","i":1}""",
            upack.Obj(
              upack.Str("$type") -> upack.Str("AnQ"),
              upack.Str("i") -> upack.Int32(1)
            ),
            upack.Obj(
              upack.Str("$type") -> upack.Str("upickletest.DeepHierarchy.AnQ"),
              upack.Str("i") -> upack.Int32(1)
            )
          )

          test - rw(
            F(AnQ(1)),
            """{"$type": "F","q":{"$type":"AnQ", "i":1}}""",
            """{"$type": "upickletest.DeepHierarchy.F","q":{"$type":"upickletest.DeepHierarchy.AnQ", "i":1}}""",
            upack.Obj(
              upack.Str("$type") -> upack.Str("F"),
              upack.Str("q") -> upack.Obj(
                upack.Str("$type") -> upack.Str("AnQ"),
                upack.Str("i") -> upack.Int32(1)
              )
            ),
            upack.Obj(
              upack.Str("$type") -> upack.Str("upickletest.DeepHierarchy.F"),
              upack.Str("q") -> upack.Obj(
                upack.Str("$type") -> upack.Str("upickletest.DeepHierarchy.AnQ"),
                upack.Str("i") -> upack.Int32(1)
              )
            )
          )
          test - rw(
            F(AnQ(2)): A,
            """{"$type": "F","q":{"$type":"AnQ", "i":2}}""",
            """{"$type": "upickletest.DeepHierarchy.F","q":{"$type":"upickletest.DeepHierarchy.AnQ", "i":2}}""",
            upack.Obj(
              upack.Str("$type") -> upack.Str("F"),
              upack.Str("q") -> upack.Obj(
                upack.Str("$type") -> upack.Str("AnQ"),
                upack.Str("i") -> upack.Int32(2)
              )
            ),
            upack.Obj(
              upack.Str("$type") -> upack.Str("upickletest.DeepHierarchy.F"),
              upack.Str("q") -> upack.Obj(
                upack.Str("$type") -> upack.Str("upickletest.DeepHierarchy.AnQ"),
                upack.Str("i") -> upack.Int32(2)
              )
            )
          )
          test - rw(
            F(AnQ(3)): C,
            """{"$type": "F","q":{"$type":"AnQ", "i":3}}""",
            """{"$type": "upickletest.DeepHierarchy.F","q":{"$type":"upickletest.DeepHierarchy.AnQ", "i":3}}""",
            upack.Obj(
              upack.Str("$type") -> upack.Str("F"),
              upack.Str("q") -> upack.Obj(
                upack.Str("$type") -> upack.Str("AnQ"),
                upack.Str("i") -> upack.Int32(3)
              )
            ),
            upack.Obj(
              upack.Str("$type") -> upack.Str("upickletest.DeepHierarchy.F"),
              upack.Str("q") -> upack.Obj(
                upack.Str("$type") -> upack.Str("upickletest.DeepHierarchy.AnQ"),
                upack.Str("i") -> upack.Int32(3)
              )
            )
          )
          test - rw(
            D("1"),
            """{"$type": "D", "s":"1"}""",
            """{"$type": "upickletest.DeepHierarchy.D", "s":"1"}""",
            upack.Obj(
              upack.Str("$type") -> upack.Str("D"),
              upack.Str("s") -> upack.Str("1")
            ),
            upack.Obj(
              upack.Str("$type") -> upack.Str("upickletest.DeepHierarchy.D"),
              upack.Str("s") -> upack.Str("1")
            )
          )
          test - rw(
            D("1"): C,
            """{"$type": "D", "s":"1"}""",
            """{"$type": "upickletest.DeepHierarchy.D", "s":"1"}""",
            upack.Obj(
              upack.Str("$type") -> upack.Str("D"),
              upack.Str("s") -> upack.Str("1")
            ),
            upack.Obj(
              upack.Str("$type") -> upack.Str("upickletest.DeepHierarchy.D"),
              upack.Str("s") -> upack.Str("1")
            )
          )
          test - rw(
            D("1"): A,
            """{"$type": "D", "s":"1"}""",
            """{"$type": "upickletest.DeepHierarchy.D", "s":"1"}""",
            upack.Obj(
              upack.Str("$type") -> upack.Str("D"),
              upack.Str("s") -> upack.Str("1")
            ),
            upack.Obj(
              upack.Str("$type") -> upack.Str("upickletest.DeepHierarchy.D"),
              upack.Str("s") -> upack.Str("1")
            )
          )
          test - rw(
            E(true),
            """{"$type": "E", "b":true}""",
            """{"$type": "upickletest.DeepHierarchy.E", "b":true}""",
            upack.Obj(
              upack.Str("$type") -> upack.Str("E"),
              upack.Str("b") -> upack.True
            ),
            upack.Obj(
              upack.Str("$type") -> upack.Str("upickletest.DeepHierarchy.E"),
              upack.Str("b") -> upack.True
            )
          )
          test - rw(
            E(true): C,
            """{"$type": "E","b":true}""",
            """{"$type": "upickletest.DeepHierarchy.E","b":true}""",
            upack.Obj(
              upack.Str("$type") -> upack.Str("E"),
              upack.Str("b") -> upack.True
            ),
            upack.Obj(
              upack.Str("$type") -> upack.Str("upickletest.DeepHierarchy.E"),
              upack.Str("b") -> upack.True
            )
          )
          test - rw(
            E(true): A,
            """{"$type": "E", "b":true}""",
            """{"$type": "upickletest.DeepHierarchy.E", "b":true}""",
            upack.Obj(
              upack.Str("$type") -> upack.Str("E"),
              upack.Str("b") -> upack.True
            ),
            upack.Obj(
              upack.Str("$type") -> upack.Str("upickletest.DeepHierarchy.E"),
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
          """ "upickletest.Singletons.BB" """,
          """{"$type":"upickletest.Singletons.BB"}""",
          upack.Str("BB"),
          upack.Str("upickletest.Singletons.BB"),
          upack.Obj(upack.Str("$type") -> upack.Str("upickletest.Singletons.BB"))
        )
        rw(
          CC,
          """ "CC" """,
          """ "upickletest.Singletons.CC" """,
          """{"$type":"upickletest.Singletons.CC"}""",
          upack.Str("CC"),
          upack.Str("upickletest.Singletons.CC"),
          upack.Obj(upack.Str("$type") -> upack.Str("upickletest.Singletons.CC"))
        )
        rw(
          BB: AA,
          """ "BB" """,
          """ "upickletest.Singletons.BB" """,
          """{"$type":"upickletest.Singletons.BB"}""",
          upack.Str("BB"),
          upack.Str("upickletest.Singletons.BB"),
          upack.Obj(upack.Str("$type") -> upack.Str("upickletest.Singletons.BB"))
        )
        rw(
          CC: AA,
          """ "CC" """,
          """ "upickletest.Singletons.CC" """,
          """{"$type":"upickletest.Singletons.CC"}""",
          upack.Str("CC"),
          upack.Str("upickletest.Singletons.CC"),
          upack.Obj(upack.Str("$type") -> upack.Str("upickletest.Singletons.CC"))
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
         """{"$type": "upickletest.SpecialChars.+1"}""",
      )
      rw(
        SpecialChars.`+1`(1),
        """{"$type": "+1", "+1": 1}""",
        """{"$type": "upickletest.SpecialChars.+1", "+1": 1}"""
      )
      rw(
        SpecialChars.`-1`(),
        """{"$type": "-1"}""",
        """{"$type": "upickletest.SpecialChars.-1"}""",
      )
      rw(
        SpecialChars.`-1`(1),
         """{"$type": "-1", "-1": 1}""",
         """{"$type": "upickletest.SpecialChars.-1", "-1": 1}""",
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
      upickle.default.read[UnknownKeys.Default]("""{"id":1, "name":"x", "omg": "wtf"}""", trace = false) ==>
        UnknownKeys.Default(1, "x")

      upickle.default.read[UnknownKeys.Allow]("""{"id":1, "name":"x", "omg": "wtf"}""", trace = false) ==>
        UnknownKeys.Allow(1, "x")

      intercept[upickle.core.AbortException]{
        upickle.default.read[UnknownKeys.DisAllow]("""{"id":1, "name":"x", "omg": "wtf"}""", trace = false)
      }

      // If the upickle API sets `override def allowUnknownKeys = false`, we default to treating unknown keys
      // as an error, `@allowUnknownKeys(false)` does nothing, but `@allowUnknownKeys(true)` makes unknown
      // keys get ignored (just for the annotated class)
      intercept[upickle.core.AbortException] {
        UnknownKeys.DisallowPickler.read[UnknownKeys.Default]("""{"id":1, "name":"x", "omg": "wtf"}""", trace = false) ==>
          UnknownKeys.Default(1, "x")
      }

      UnknownKeys.DisallowPickler.read[UnknownKeys.Allow]("""{"id":1, "name":"x", "omg": "wtf"}""", trace = false) ==>
        UnknownKeys.Allow(1, "x")

      intercept[upickle.core.AbortException]{
        UnknownKeys.DisallowPickler.read[UnknownKeys.DisAllow]("""{"id":1, "name":"x", "omg": "wtf"}""", trace = false)
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
      compileError("upickle.default.macroRW[upickletest.MultiKeyedObj.type]")
        .check("", "inherits from multiple parent types with different discriminator keys")

      compileError("upickle.default.macroRW[upickletest.SomeMultiKeyedObj.type]")
        .check("", "inherits from multiple parent types with different discriminator keys")
    }

    test("multiKeyedADT") {
      compileError("upickle.default.macroRW[upickletest.MultiKeyedObj.type]")
        .check("", "inherits from multiple parent types with different discriminator keys")

      compileError("upickle.default.macroRW[upickletest.SomeMultiKeyedObj.type]")
        .check("", "inherits from multiple parent types with different discriminator keys")
    }

    test("objectTypeKeyWriteFullyQualified") {
      object customPickler extends upickle.AttributeTagged {
        override def objectTypeKeyWriteFullyQualified = true
      }

      val customPicklerTest = new TestUtil(customPickler)

      implicit def rwB: customPickler.ReadWriter[upickletest.Hierarchy.B] = customPickler.macroRW
      implicit def rwC: customPickler.ReadWriter[upickletest.Hierarchy.C] = customPickler.macroRW

      // Make sure both custom pickler and default pickler can read both long and short `$type` tags,
      // but that the custom pickler generates the long `$type` tag while the default pickler
      // generates the short one
      customPicklerTest.rw(
        new Hierarchy.B(1),
        """{"$type": "upickletest.Hierarchy.B", "i": 1}""",
        """{"$type": "B", "i": 1}"""
      )
      rw(
        new Hierarchy.B(1),
        """{"$type": "B", "i": 1}""",
        """{"$type": "upickletest.Hierarchy.B", "i": 1}"""
      )

      customPicklerTest.rw(
        new Hierarchy.C("x", "y"),
        """{"$type": "upickletest.Hierarchy.C", "s1": "x", "s2": "y"}""",
         """{"$type": "C", "s1": "x", "s2": "y"}"""
      )
      rw(
        new Hierarchy.C("x", "y"),
        """{"$type": "C", "s1": "x", "s2": "y"}""",
        """{"$type": "upickletest.Hierarchy.C", "s1": "x", "s2": "y"}"""
      )

    }

    test("tagName"){
      val customPicklerTest = new TestUtil(TagName.TagNamePickler)
      customPicklerTest.rw(
        TagName.Bar(123),
        """{"_tag": "Bar", "x": 123}"""
      )

    }

    test("sealedClass"){
      assert(write(SealedClass(3, "Hello")) == """{"$type":"SealedClass","i":3,"s":"Hello"}""")
    }

    test("flatten"){
      import Flatten._
      val a = FlattenTest(10, "test", Nested(3.0, Map("one" -> 1, "two" -> 2)), Nested2("hello"))
      rw(a, """{"i":10,"s":"test","d":3,"one":1,"two":2,"name":"hello"}""")
    }

    test("flattenTypeParam"){
      import Flatten._
      val a = FlattenTestWithType[Nested](10, Nested(5.0, Map("one" -> 1, "two" -> 2)))
      rw(a, """{"i":10,"d":5,"one":1,"two":2}""")
    }

    test("nestedFlatten") {
      import Flatten._
      val value = Outer(1.1, Inner(InnerMost("test", 42), true))
      rw(value, """{"d":1.1,"a":"test","b":42,"c":true}""")
    }

    test("flattenWithMap") {
      import Flatten._
      val value = HasMap(Map("key1" -> "value1", "key2" -> "value2"), 10)
      rw(value, """{"key1":"value1","key2":"value2","i":10}""")
    }

    test("flattenEmptyMap") {
      import Flatten._
      val value = HasMap(Map.empty, 10)
      rw(value, """{"i":10}""")
      write(1)
    }

    test("flattenWithDefaults") {
      import Flatten._
      val value = FlattenWithDefault(10, NestedWithDefault(l = "default"))
      rw(value, """{"i":10,"l":"default"}""")
    }

    test("flattenSeq") {
      import Flatten._
      val value = FlattenSeq(Seq("a" -> 1, "b" -> 2))
      rw(value, """{"a":1,"b":2}""")
    }

     test("flattenLinkedHashMap") {
       import Flatten._
       val value = Collection(scala.collection.mutable.LinkedHashMap("a" -> ValueClass(3.0), "b" -> ValueClass(4.0)))
       rw(value, """{"a":{"value":3},"b":{"value":4}}""")
     }

    test("flattenIntKey") {
      import Flatten._
      val value = FlattenIntKey(10, Map(1 -> "one", 2 -> "two"))
      rw(value, """{"i":10,"1":"one","2":"two"}""")
    }

    test("flattenSeqIntKey") {
      import Flatten._
      val value = FlattenSeqIntKey(Seq(1 -> "one", 2 -> "two"))
      rw(value, """{"1":"one","2":"two"}""")
    }

    test("flattenLongKey") {
      import Flatten._
      val value = FlattenLongKey(Map(100L -> 1, 200L -> 2))
      rw(value, """{"100":1,"200":2}""")
    }

    test("flattenBufferedValueKey") {
      import Flatten.BufferedValueKey._
      import upickle.core.BufferedValue

      // Read JSON and verify that keys are BufferedValue.Str with correct index values
      // Index is the position of the opening quote character
      // {"foo":"value1","bar":"value2"}
      // 0         1         2         3
      // 0123456789012345678901234567890
      val json = """{"foo":"value1","bar":"value2"}"""
      val result = upickle.default.read[FlattenBufferedKey](json)

      // Get the keys and verify they are BufferedValue.Str with appropriate indices
      val keys = result.m.keys.toList.sortBy(_.asInstanceOf[BufferedValue.Str].value0.toString)
      assert(keys.length == 2)

      // "bar" key starts at index 16 (the opening quote)
      val barKey = keys(0).asInstanceOf[BufferedValue.Str]
      assert(barKey.value0.toString == "bar")
      assert(barKey.index == 16)

      // "foo" key starts at index 1 (the opening quote)
      val fooKey = keys(1).asInstanceOf[BufferedValue.Str]
      assert(fooKey.value0.toString == "foo")
      assert(fooKey.index == 1)

      // Verify round-trip works
      val written = upickle.default.write(result)
      val reread = upickle.default.read[FlattenBufferedKey](written)
      assert(reread.m.size == result.m.size)
    }
  }
}
