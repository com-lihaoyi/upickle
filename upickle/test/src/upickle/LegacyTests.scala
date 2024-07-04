package upickle
import utest._
import LegacyTestUtil.rw

import upickle.legacy.{ReadWriter => RW, Reader => R, Writer => W}
object LegacyTests extends TestSuite {

  val tests = Tests {
    test("simpleAdt"){
      implicit def ADT0rw: RW[ADTs.ADT0] = upickle.legacy.macroRW
      implicit def ADTarw: RW[ADTs.ADTa] = upickle.legacy.macroRW
      implicit def ADTbrw: RW[ADTs.ADTb] = upickle.legacy.macroRW
      implicit def ADTcrw: RW[ADTs.ADTc] = upickle.legacy.macroRW
      implicit def ADTdrw: RW[ADTs.ADTd] = upickle.legacy.macroRW
      implicit def ADTerw: RW[ADTs.ADTe] = upickle.legacy.macroRW
      implicit def ADTfrw: RW[ADTs.ADTf] = upickle.legacy.macroRW
      implicit def ADTzrw: RW[ADTs.ADTz] = upickle.legacy.macroRW

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
      import Hierarchy._
      implicit def Brw: RW[B] = upickle.legacy.macroRW
      implicit def Crw: RW[C] = upickle.legacy.macroRW
      implicit def Arw: RW[A] = upickle.legacy.ReadWriter.merge(Crw, Brw)
      implicit def AnZrw: RW[AnZ.type] = upickle.legacy.macroRW
      implicit def Zrw: RW[Z] = upickle.legacy.macroRW
      test("shallow"){
        test - rw(B(1), """["B",{"i":1}]""")
        test - rw(C("a", "b"), """["C",{"s1":"a","s2":"b"}]""")

        test - rw(AnZ: Z, """["AnZ",{}]""")
        test - rw(AnZ, """["AnZ",{}]""")

        test - rw(Hierarchy.B(1): Hierarchy.A, """["B", {"i":1}]""")
        test - rw(C("a", "b"): A, """["C",{"s1":"a","s2":"b"}]""")
      }

      test("deep"){
        import DeepHierarchy._
        implicit def Arw: RW[A] = upickle.legacy.macroRW
        implicit def Brw: RW[B] = upickle.legacy.macroRW
        implicit def Crw: RW[C] = upickle.legacy.macroRW
        implicit def AnQrw: RW[AnQ] = upickle.legacy.macroRW
        implicit def Qrw: RW[Q] = upickle.legacy.macroRW
        implicit def Drw: RW[D] = upickle.legacy.macroRW
        implicit def Erw: RW[E] = upickle.legacy.macroRW
        implicit def Frw: RW[F] = upickle.legacy.macroRW
        test - rw(B(1), """["B",{"i":1}]""")
        test - rw(B(1): A, """["B",{"i":1}]""")
        test - rw(AnQ(1): Q, """["AnQ",{"i":1}]""")
        test - rw(AnQ(1), """["AnQ",{"i":1}]""")

        test - rw(F(AnQ(1)), """["F",{"q":["AnQ",{"i":1}]}]""")
        test - rw(F(AnQ(2)): A, """["F",{"q":["AnQ",{"i":2}]}]""")
        test - rw(F(AnQ(3)): C, """["F",{"q":["AnQ",{"i":3}]}]""")
        test - rw(D("1"), """["D",{"s":"1"}]""")
        test - rw(D("1"): C, """["D",{"s":"1"}]""")
        test - rw(D("1"): A, """["D",{"s":"1"}]""")
        test - rw(E(true), """["E",{"b":true}]""")
        test - rw(E(true): C, """["E",{"b":true}]""")
        test - rw(E(true): A, """["E",{"b":true}]""")
      }
    }
    test("singleton"){
      import Singletons._

      implicit def AArw: RW[AA] = legacy.macroRW
      implicit def BBrw: RW[BB.type] = legacy.macroRW
      implicit def CCrw: RW[CC.type] = legacy.macroRW
      rw(BB, """["BB",{}]""")
      rw(CC, """["CC",{}]""")
      rw(BB: AA, """["BB",{}]""")
      rw(CC: AA, """["CC",{}]""")
    }
    test("robustnessAgainstVaryingSchemas"){
      test("renameKeysViaAnnotations"){
        import Annotated._
        implicit def Arw: RW[A] = upickle.legacy.macroRW
        implicit def Brw: RW[B] = upickle.legacy.macroRW
        implicit def Crw: RW[C] = upickle.legacy.macroRW
        test - rw(B(1), """["0", {"omg":1}]""")
        test - rw(C("a", "b"), """["1", {"lol":"a","wtf":"b"}]""")

        test - rw(B(1): A, """["0", {"omg":1}]""")
        test - rw(C("a", "b"): A, """["1", {"lol":"a","wtf":"b"}]""")
      }
      test("useDefaults"){
        // Ignore the values which match the default when writing and
        // substitute in defaults when reading if the key is missing
        import Defaults._
        implicit def Arw: RW[ADTa] = upickle.legacy.macroRW
        implicit def Brw: RW[ADTb] = upickle.legacy.macroRW
        implicit def Crw: RW[ADTc] = upickle.legacy.macroRW
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
        implicit def ADTarw: RW[ADTs.ADTa] = upickle.legacy.macroRW
        implicit def ADTbrw: RW[ADTs.ADTb] = upickle.legacy.macroRW

        val r1 = upickle.legacy.read[ADTa]( """{"i":123, "j":false, "k":"haha"}""")
        assert(r1 == ADTa(123))
        val r2 = upickle.legacy.read[ADTb]( """{"i":123, "j":false, "k":"haha", "s":"kk", "l":true, "z":[1, 2, 3]}""")
        assert(r2 == ADTb(123, "kk"))
      }
    }

    test("generics"){
      import GenericADTs._
      test - {
        val D1 = Delta
        implicit def Insertrw[A: R: W, B: R: W]: RW[D1.Insert[A, B]] = upickle.legacy.macroRW
        implicit def Removerw[A: R: W]: RW[D1.Remove[A]] = upickle.legacy.macroRW
        implicit def Clearrw: RW[D1.Clear] = upickle.legacy.macroRW
        implicit def D1rw[A: R: W, B: R: W]: RW[D1[A, B]] = upickle.legacy.macroRW
        type D1[+A, +B] = Delta[A, B]
        rw(D1.Insert(1, 1), s"""["Insert",{"key":1,"value":1}]""")
        rw(D1.Insert(1, 1): D1[Int, Int], s"""["Insert",{"key":1,"value":1}]""")
        rw(D1.Remove(1), s"""["Remove",{"key":1}]""")
        rw(D1.Remove(1): D1[Int, Int], s"""["Remove",{"key":1}]""")
        rw(D1.Clear(), s"""["Clear",{}]""")
        rw(D1.Clear(): D1[Int, Int], s"""["Clear",{}]""")
      }
      test - {
        val D2 = DeltaInvariant
        type D2[A, B] = DeltaInvariant[A, B]
        implicit def Insertrw[A: R: W, B: R: W]: RW[D2.Insert[A, B]] = upickle.legacy.macroRW
        implicit def Removerw[A: R: W, B]: RW[D2.Remove[A, B]] = upickle.legacy.macroRW
        implicit def Clearrw[A, B]: RW[D2.Clear[A, B]] = upickle.legacy.macroRW
        implicit def D2rw[A: R: W, B: R: W]: RW[D2[A, B]] = upickle.legacy.macroRW
        rw(D2.Insert(1, 1), s"""["Insert",{"key":1,"value":1}]""")
        rw(D2.Insert(1, 1): D2[Int, Int], s"""["Insert",{"key":1,"value":1}]""")
        rw(D2.Remove(1), s"""["Remove",{"key":1}]""")
        rw(D2.Remove(1): D2[Int, Int], s"""["Remove",{"key":1}]""")
        rw(D2.Clear(), s"""["Clear",{}]""")
        rw(D2.Clear(): D2[Int, Int], s"""["Clear",{}]""")
      }
    }
    test("recursiveDataTypes"){
      import Recursive._
      implicit def IntTreerw: RW[IntTree] = upickle.legacy.macroRW
      implicit def SingleNoderw: RW[SingleNode] = upickle.legacy.macroRW

      implicit def SingleTreerw: RW[SingleTree] = upickle.legacy.macroRW

      implicit def Noderw: RW[Node] = upickle.legacy.macroRW


      implicit def Endrw: RW[End.type] = upickle.legacy.macroRW
      implicit def LLrw: RW[LL] = upickle.legacy.macroRW

      rw(
        IntTree(123, List(IntTree(456, Nil), IntTree(789, Nil))),
        """{"value":123,"children":[{"value":456,"children":[]},{"value":789,"children":[]}]}"""
      )
      rw(
        SingleNode(123, List(SingleNode(456, Nil), SingleNode(789, Nil))),
        """["SingleNode",{"value":123,"children":[["SingleNode",{"value":456,"children":[]}],["SingleNode",{"value":789,"children":[]}]]}]"""
      )
      rw(
        SingleNode(123, List(SingleNode(456, Nil), SingleNode(789, Nil))): SingleTree,
        """["SingleNode",{"value":123,"children":[["SingleNode",{"value":456,"children":[]}],["SingleNode",{"value":789,"children":[]}]]}]"""
      )
      rw(End: LL, """["End",{}]""")
      rw(Node(3, End): LL, """["Node",{"c":3,"next":["End",{}]}]""")
      rw(Node(6, Node(3, End)), """["Node",{"c":6,"next":["Node",{"c":3,"next":["End",{}]}]}]""")

    }
    test("varargs"){
      implicit def IntTreerw: RW[Varargs.Sentence] = upickle.legacy.macroRW
      rw(Varargs.Sentence("a", "b", "c"), """{"a":"a","bs":["b","c"]}""")
      rw(Varargs.Sentence("a"), """{"a":"a","bs":[]}""")
    }


    test("issues"){
      test("issue95"){
        implicit def rw1: RW[C1] = legacy.macroRW
        implicit def rw2: RW[C2] = legacy.macroRW
        implicit def rw3: RW[GeoCoding2] = legacy.macroRW
        implicit def rw4: RW[Result2] = legacy.macroRW
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
      test("scalatex"){
        implicit def rw1: RW[Ast] = legacy.macroRW
        implicit def rw2: RW[Ast.Block] = legacy.macroRW
        implicit def rw3: RW[Ast.Block.Sub] = legacy.macroRW
        implicit def rw4: RW[Ast.Block.Text] = legacy.macroRW
        implicit def rw5: RW[Ast.Block.For] = legacy.macroRW
        implicit def rw6: RW[Ast.Block.IfElse] = legacy.macroRW
        implicit def rw7: RW[Ast.Header] = legacy.macroRW
        implicit def rw8: RW[Ast.Chain] = legacy.macroRW
        implicit def rw9: RW[Ast.Chain.Sub] = legacy.macroRW
        implicit def rw10: RW[Ast.Chain.Prop] = legacy.macroRW
        implicit def rw11: RW[Ast.Chain.TypeArgs] = legacy.macroRW
        implicit def rw12: RW[Ast.Chain.Args] = legacy.macroRW
        val block = Ast.Block(1, Seq(Ast.Block.Text(2, "hello")))
        val blockText = """[
          "Block",
          {
            "offset":1,
            "parts":[
              [
                "Block.Text",
                {
                  "offset":2,
                  "txt":"hello"
                }
              ]
            ]
          }
        ]"""
        rw(block: Ast, blockText)
        rw(block: Ast.Block, blockText)
        rw(block: Ast.Block.Sub, blockText)
        rw(block: Ast.Chain.Sub, blockText)

        val header = Ast.Header(0, "Hello", block)
        val headerText = s"""[
          "Header",
          {
            "offset": 0,
            "front": "Hello",
            "block": $blockText
          }
        ]"""
        rw(header: Ast, headerText)
        rw(header: Ast.Header, headerText)
        rw(header: Ast.Block.Sub, headerText)
        rw(header: Ast.Chain.Sub, headerText)
      }
//      test("companionImplicitPickedUp"){
//        assert(implicitly[upickle.default.Reader[TypedFoo]] eq TypedFoo.readWriter)
//        assert(implicitly[upickle.default.Writer[TypedFoo]] eq TypedFoo.readWriter)
//        assert(implicitly[upickle.default.ReadWriter[TypedFoo]] eq TypedFoo.readWriter)
//      }
  //    test("companionImplicitWorks"){
  //
  //      rw(TypedFoo.Bar(1): TypedFoo, """{"$type": "upickle.TypedFoo.Bar", "i": 1}""")
  //      rw(TypedFoo.Baz("lol"): TypedFoo, """{"$type": "upickle.TypedFoo.Baz", "s": "lol"}""")
  //      rw(TypedFoo.Quz(true): TypedFoo, """{"$type": "upickle.TypedFoo.Quz", "b": true}""")
  //    }
    }
    test("jsonInCaseClass"){

      implicit def arw: RW[CaseClassWithJson] = upickle.legacy.macroRW
      rw(new CaseClassWithJson(ujson.Num(7)), """{"json":7}""")
      rw(new CaseClassWithJson(ujson.Arr(ujson.Num(7), ujson.Str("lol"))), """{"json":[7,"lol"]}""")
    }
    test("traitFromOtherPackage"){
      implicit val BaseChildRW: RW[subpackage.Base.Child.type] = upickle.legacy.macroRW
      implicit val BaseRW: RW[subpackage.Base] = upickle.legacy.macroRW
      implicit val WrapperRW: RW[subpackage.Wrapper] = upickle.legacy.macroRW
      upickle.legacy.write(subpackage.Wrapper(subpackage.Base.Child))
    }
  }
}
