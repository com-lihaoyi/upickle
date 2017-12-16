package upickle
import acyclic.file
import utest._
import upickle.TestUtil._
import upickle.default.{read, write}

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
    implicit val thing2Writer = upickle.default.Writer[T]{
      case t => Js.Str(t.i + " " + t.s)
    }
    implicit val thing2Reader = upickle.default.Reader[T]{
      case Js.Str(str) =>
        val Array(i, s) = str.toString.split(" ")
        f(i.toInt, s)
    }
  }
  object Thing2 extends ThingBaseCompanion[Thing2](new Thing2(_, _))

  case class Thing3(i: Int, s: String) extends ThingBase

  object Thing3 extends ThingBaseCompanion[Thing3](new Thing3(_, _))
}

// this can be un-sealed as long as `derivedSubclasses` is defined in the companion
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

object MacroTests extends TestSuite {
  import Generic.ADT
  import Hierarchy._
  import Recursive._
  import Defaults._
  import ADTs.ADT0
  type Data = ADT[Seq[(Int, Int)], String, A, LL, ADTc, ADT0]
  val data: Data = ADT(
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
  // Doesn't work :(
//  case class A_(objects: Option[C_]); case class C_(nodes: Option[C_])

//  implicitly[Reader[A_]]
//  implicitly[upickle.old.Writer[upickle.MixedIn.Obj.ClsB]]
//  println(write(ADTs.ADTc(1, "lol", (1.1, 1.2))))
//  implicitly[upickle.old.Writer[ADTs.ADTc]]

  val tests = Tests {

    'mixedIn{
      import MixedIn._
      * - rw(Obj.ClsB(1), """{"i":1}""")
      * - rw(Obj.ClsA("omg"), """{"s":"omg"}""")
     }

    /*
    // TODO Currently not supported
    'declarationWithinFunction {
      sealed trait Base
      case object Child extends Base
      case class Wrapper(base: Base)
      * - upickle.write(Wrapper(Child))
    }

    'traitFromOtherPackage {
      * - upickle.write(subpackage.Wrapper(subpackage.Base.Child))
    }
    */
    'exponential{

      // Doesn't even need to execute, as long as it can compile
      val ww1 = implicitly[upickle.default.Writer[Exponential.A1]]
    }


    'commonCustomStructures{
      'simpleAdt {

        * - rw(ADTs.ADT0(), """{
        }""")
        * - rw(ADTs.ADTa(1), """{"i":1}""")
        * - rw(ADTs.ADTb(1, "lol"), """{"i":1,"s":"lol"}""")

        * - rw(ADTs.ADTc(1, "lol", (1.1, 1.2)), """{"i":1,"s":"lol","t":[1.1,1.2]}""")
        * - rw(
          ADTs.ADTd(1, "lol", (1.1, 1.2), ADTs.ADTa(1)),
          """{"i":1,"s":"lol","t":[1.1,1.2],"a":{"i":1}}"""
        )

        * - rw(
          ADTs.ADTe(1, "lol", (1.1, 1.2), ADTs.ADTa(1), List(1.2, 2.1, 3.14)),
          """{"i":1,"s":"lol","t":[1.1,1.2],"a":{"i":1},"q":[1.2,2.1,3.14]}"""
        )

        * - rw(
          ADTs.ADTf(1, "lol", (1.1, 1.2), ADTs.ADTa(1), List(1.2, 2.1, 3.14), Some(None)),
          """{"i":1,"s":"lol","t":[1.1,1.2],"a":{"i":1},"q":[1.2,2.1,3.14],"o":[[]]}"""
        )
        val chunks = for (i <- 1 to 18) yield {
          val rhs = if (i % 2 == 1) "1" else "\"1\""
          val lhs = '"' + s"t$i" + '"'
          s"$lhs:$rhs"
        }

        val expected = s"""{${chunks.mkString(",")}}"""
        * - rw(
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
          * - rw(B(1), """{"$type": "upickle.Hierarchy.B", "i":1}""")
          * - rw(C("a", "b"), """{"$type": "upickle.Hierarchy.C", "s1":"a","s2":"b"}""")
//Doesn't work in 2.10.4
//          * - rw(AnZ: Z, """["upickle.Hierarchy.AnZ",{}]""")
//          * - rw(AnZ, """["upickle.Hierarchy.AnZ",{}]""")

          * - rw(Hierarchy.B(1): Hierarchy.A, """{"$type": "upickle.Hierarchy.B", "i":1}""")
          * - rw(C("a", "b"): A, """{"$type": "upickle.Hierarchy.C", "s1":"a","s2":"b"}""")
        }
        'deep{
          import DeepHierarchy._

          * - rw(B(1), """{"$type": "upickle.DeepHierarchy.B", "i":1}""")
          * - rw(B(1): A, """{"$type": "upickle.DeepHierarchy.B", "i":1}""")
          * - rw(AnQ(1): Q, """{"$type": "upickle.DeepHierarchy.AnQ", "i":1}""")
          * - rw(AnQ(1), """{"$type": "upickle.DeepHierarchy.AnQ","i":1}""")

          * - rw(F(AnQ(1)), """{"$type": "upickle.DeepHierarchy.F","q":{"$type":"upickle.DeepHierarchy.AnQ", "i":1}}""")
          * - rw(F(AnQ(2)): A, """{"$type": "upickle.DeepHierarchy.F","q":{"$type":"upickle.DeepHierarchy.AnQ", "i":2}}""")
          * - rw(F(AnQ(3)): C, """{"$type": "upickle.DeepHierarchy.F","q":{"$type":"upickle.DeepHierarchy.AnQ", "i":3}}""")
          * - rw(D("1"), """{"$type": "upickle.DeepHierarchy.D", "s":"1"}""")
          * - rw(D("1"): C, """{"$type": "upickle.DeepHierarchy.D", "s":"1"}""")
          * - rw(D("1"): A, """{"$type": "upickle.DeepHierarchy.D", "s":"1"}""")
          * - rw(E(true), """{"$type": "upickle.DeepHierarchy.E", "b":true}""")
          * - rw(E(true): C, """{"$type": "upickle.DeepHierarchy.E","b":true}""")
          * - rw(E(true): A, """{"$type": "upickle.DeepHierarchy.E", "b":true}""")
        }
      }
      'singleton {
        import Singletons._

        rw(BB, """{"$type":"upickle.Singletons.BB"}""")
        rw(CC, """{"$type":"upickle.Singletons.CC"}""")
        rw(BB: AA, """{"$type":"upickle.Singletons.BB"}""")
        rw(CC: AA, """{"$type":"upickle.Singletons.CC"}""")
      }
    }
    'robustnessAgainstVaryingSchemas {
      'renameKeysViaAnnotations {
        import Annotated._

        * - rw(B(1), """{"$type": "0", "omg":1}""")
        * - rw(C("a", "b"), """{"$type": "1", "lol":"a","wtf":"b"}""")

        * - rw(B(1): A, """{"$type": "0", "omg":1}""")
        * - rw(C("a", "b"): A, """{"$type": "1", "lol":"a","wtf":"b"}""")
      }
      'useDefaults {
        // Ignore the values which match the default when writing and
        // substitute in defaults when reading if the key is missing
        import Defaults._
        * - rw(ADTa(), "{}")
        * - rw(ADTa(321), """{"i":321}""")
        * - rw(ADTb(s = "123"), """{"s":"123"}""")
        * - rw(ADTb(i = 234, s = "567"), """{"i":234,"s":"567"}""")
        * - rw(ADTc(s = "123"), """{"s":"123"}""")
        * - rw(ADTc(i = 234, s = "567"), """{"i":234,"s":"567"}""")
        * - rw(ADTc(t = (12.3, 45.6), s = "789"), """{"s":"789","t":[12.3,45.6]}""")
        * - rw(ADTc(t = (12.3, 45.6), s = "789", i = 31337), """{"i":31337,"s":"789","t":[12.3,45.6]}""")
      }
      'ignoreExtraFieldsWhenDeserializing {
        import ADTs._
        val r1 = read[ADTa]( """{"i":123, "j":false, "k":"haha"}""")
        assert(r1 == ADTa(123))
        val r2 = read[ADTb]( """{"i":123, "j":false, "k":"haha", "s":"kk", "l":true, "z":[1, 2, 3]}""")
        assert(r2 == ADTb(123, "kk"))
      }
    }

    'GenericDataTypes{
      'simple {
        import Generic.A
        * - rw(A(1), """{"t":1}""")
        * - rw(A("1"), """{"t":"1"}""")
        * - rw(A(Seq("1", "2", "3")), """{"t":["1","2","3"]}""")
        * - rw(A(A(A(A(A(A(A(1))))))), """{"t":{"t":{"t":{"t":{"t":{"t":{"t":1}}}}}}}""")
      }
      'large{
        import Generic.ADT
        rw(ADT(1, 2, 3, 4, 5, 6), """{"a":1,"b":2,"c":3,"d":4,"e":5,"f":6}""")
        rw(
          ADT(
            ADT(1, 2, 3, 4, 5, 6),
            ADT(1, 2, 3, 4, 5, 6),
            ADT(1, 2, 3, 4, 5, 6),
            ADT(1, 2, 3, 4, 5, 6),
            ADT(1, 2, 3, 4, 5, 6),
            ADT(1, 2, 3, 4, 5, 6)
          ),
          """{"a":{"a":1,"b":2,"c":3,"d":4,"e":5,"f":6},"b":{"a":1,"b":2,"c":3,"d":4,"e":5,"f":6},"c":{"a":1,"b":2,"c":3,"d":4,"e":5,"f":6},"d":{"a":1,"b":2,"c":3,"d":4,"e":5,"f":6},"e":{"a":1,"b":2,"c":3,"d":4,"e":5,"f":6},"f":{"a":1,"b":2,"c":3,"d":4,"e":5,"f":6}}"""
        )
      }
      'ADT{
        import GenericADTs._
        * - {
          val pref1 = "upickle.GenericADTs.Delta"
          val D1 = Delta
          type D1[+A, +B] = Delta[A, B]
          rw(D1.Insert(1, 1), s"""{"$$type":"$pref1.Insert","key":1,"value":1}""")
          rw(D1.Insert(1, 1): D1[Int, Int], s"""{"$$type":"$pref1.Insert","key":1,"value":1}""")
          rw(D1.Remove(1), s"""{"$$type":"$pref1.Remove","key":1}""")
          rw(D1.Remove(1): D1[Int, Int], s"""{"$$type":"$pref1.Remove","key":1}""")
          rw(D1.Clear(), s"""{"$$type":"$pref1.Clear"}""")
          rw(D1.Clear(): D1[Int, Int], s"""{"$$type":"$pref1.Clear"}""")
        }
        * - {
          val pref2 = "upickle.GenericADTs.DeltaInvariant"
          val D2 = DeltaInvariant
          type D2[A, B] = DeltaInvariant[A, B]
          rw(D2.Insert(1, 1), s"""{"$$type":"$pref2.Insert","key":1,"value":1}""")
          rw(D2.Insert(1, 1): D2[Int, Int], s"""{"$$type":"$pref2.Insert","key":1,"value":1}""")
          rw(D2.Remove(1), s"""{"$$type":"$pref2.Remove","key":1}""")
          rw(D2.Remove(1): D2[Int, Int], s"""{"$$type":"$pref2.Remove","key":1}""")
          rw(D2.Clear(), s"""{"$$type":"$pref2.Clear"}""")
          rw(D2.Clear(): D2[Int, Int], s"""{"$$type":"$pref2.Clear"}""")
        }
      }
    }

    'recursiveDataTypes{
      import Recursive._
      rw(
        IntTree(123, List(IntTree(456, Nil), IntTree(789, Nil))),
        """{
          "value": 123,
          "children": [
            {"value":456,"children":[]},
            {"value":789,"children":[]}
          ]
        }"""
      )
      rw(
        SingleNode(123, List(SingleNode(456, Nil), SingleNode(789, Nil))),
        """{
          "$type": "upickle.Recursive.SingleNode",
          "value": 123,
          "children": [
            {
              "$type": "upickle.Recursive.SingleNode",
              "value": 456,
              "children": []
            },
            {
              "$type": "upickle.Recursive.SingleNode",
              "value":789,
              "children":[]
            }
          ]
        }"""
      )
      rw(
        SingleNode(123, List(SingleNode(456, Nil), SingleNode(789, Nil))): SingleTree,
        """{
          "$type": "upickle.Recursive.SingleNode",
          "value": 123,
          "children": [
            {
              "$type": "upickle.Recursive.SingleNode",
              "value": 456,
              "children": []
            },
            {
              "$type": "upickle.Recursive.SingleNode",
              "value":789,
              "children":[]
            }
          ]
        }"""
      )
      rw(End: LL, """{"$type":"upickle.Recursive.End"}""")
      rw(Node(3, End): LL,
        """{
          "$type": "upickle.Recursive.Node",
          "c": 3,
          "next": {"$type":"upickle.Recursive.End"}
        }""")
      rw(Node(6, Node(3, End)),
        """{
          "$type": "upickle.Recursive.Node",
          "c": 6,
          "next": {
            "$type": "upickle.Recursive.Node",
            "c":3,
            "next":{"$type":"upickle.Recursive.End"}
          }
        }""")

    }

    'custom {
      'clsReaderWriter {
        rw(new Custom.Thing2(1, "s"), """ "1 s" """)
        rw(new Custom.Thing2(10, "sss"), """ "10 sss" """)
      }
      'caseClsReaderWriter {
        rw(new Custom.Thing3(1, "s"), """ "1 s" """)
        rw(new Custom.Thing3(10, "sss"), """ "10 sss" """)
      }
    }
    'varargs{
      rw(Varargs.Sentence("a", "b", "c"), """{"a":"a","bs":["b","c"]}""")
      rw(Varargs.Sentence("a"), """{"a":"a","bs":[]}""")
    }
      'performance{
        import Generic.ADT
        import Hierarchy._
        import Recursive._
        import Defaults._
        import ADTs.ADT0

        // Some arbitrary data that represents a mix of all the different
        // ways things can be pickled and unpickled

        val stringified = write(data)
        val r1 = read[Data](stringified)
        assert(data == r1)
        val rewritten = write(read[Data](stringified))
        assert(stringified == rewritten)

        'read{
          var n = 0
          val start = System.currentTimeMillis()
          while(System.currentTimeMillis() < start + 5000){
            read[Data](stringified)
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
    'issues {
      'issue95 {
        rw(
          Tuple1(List(C1("hello", List("world")))),
          """[[{"name": "hello", "types": ["world"]}]]"""
        )
        rw(
          C2(List(C1("hello", List("world")))),
          """{"results": [{"name": "hello", "types": ["world"]}]}"""
        )

        rw(
          GeoCoding2(List(Result2("a", "b", List("c"))), "d"),
          """{"results": [{"name": "a", "whatever": "b", "types": ["c"]}], "status": "d"}"""
        )
      }
      'scalatex{
        val block = Ast.Block(1, Seq(Ast.Block.Text(2, "hello")))
        val blockText = """{
            "$type":"upickle.Ast.Block",
            "offset":1,
            "parts":[
              {
                "$type": "upickle.Ast.Block.Text",
                "offset":2,
                "txt":"hello"
              }
            ]
          }"""
        rw(block: Ast, blockText)
        rw(block: Ast.Block, blockText)
        rw(block: Ast.Block.Sub, blockText)
        rw(block: Ast.Chain.Sub, blockText)

        val header = Ast.Header(0, "Hello", block)
        val headerText = s"""{
          "$$type": "upickle.Ast.Header",
          "offset": 0,
          "front": "Hello",
          "block": $blockText
        }"""
        rw(header: Ast, headerText)
        rw(header: Ast.Header, headerText)
        rw(header: Ast.Block.Sub, headerText)
        rw(header: Ast.Chain.Sub, headerText)
      }
      'companionImplicitPickedUp{
        assert(implicitly[upickle.default.Reader[TypedFoo]] eq TypedFoo.readWriter)
        assert(implicitly[upickle.default.Writer[TypedFoo]] eq TypedFoo.readWriter)
        assert(implicitly[upickle.default.ReadWriter[TypedFoo]] eq TypedFoo.readWriter)
      }
      'companionImplicitWorks{

        rw(TypedFoo.Bar(1): TypedFoo, """{"$type": "upickle.TypedFoo.Bar", "i": 1}""")
        rw(TypedFoo.Baz("lol"): TypedFoo, """{"$type": "upickle.TypedFoo.Baz", "s": "lol"}""")
        rw(TypedFoo.Quz(true): TypedFoo, """{"$type": "upickle.TypedFoo.Quz", "b": true}""")
      }
    }
  }
}
