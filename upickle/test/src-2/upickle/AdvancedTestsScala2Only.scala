package upickle
import utest._
import upickle.TestUtil.rw

object AdvancedTestsScala2Only extends TestSuite {
  import All._
  val tests = Tests {
    "complexTraits" - {
      val reader = implicitly[upickle.default.Reader[Outers]]
      val writer = implicitly[upickle.default.Writer[Outers]]
      assert(reader != null)
      assert(writer != null)
    }
    test("GenericDataTypes"){
      test("simple"){
        import Generic.A
        test - rw(A(Seq("1", "2", "3")), """{"t":["1","2","3"]}""")
      }
    }

    test("gadt"){
      test("simple"){
        test - rw(Gadt.Exists("hello"): Gadt[_], """{"$type":"Exists","path":"hello"}""")
        test - rw(Gadt.IsDir(" "): Gadt[_], """{"$type":"IsDir","path":" "}""")
        test - rw(Gadt.ReadBytes("\""): Gadt[_], """{"$type":"ReadBytes","path":"\""}""")
        test - rw(Gadt.CopyOver(Seq(1, 2, 3), ""): Gadt[_], """{"$type":"CopyOver","src":[1,2,3],"path":""}""")
      }
      test("partial"){
        test - rw(Gadt2.Exists("hello"): Gadt2[_, String], """{"$type":"Exists","v":"hello"}""")
        test - rw(Gadt2.IsDir(123): Gadt2[_, Int], """{"$type":"IsDir","v":123}""")
        test - rw(Gadt2.ReadBytes('h'): Gadt2[_, Char], """{"$type":"ReadBytes","v":"h"}""")
        test - rw(Gadt2.CopyOver(Seq(1, 2, 3), ""): Gadt2[_, Unit], """{"$type":"CopyOver","src":[1,2,3],"v":""}""")
      }
    }

  }
}
