package upickle.implicits.namedTuples
import utest.*

object BasicTests extends TestSuite {
  val basic_formatted = """[
    {"x":23,"y":7.5,"z":500000000000},
    {"name":"Alice","isHuman":true,"isAlien":false},
    {"arr":[1,2,3],"optionalAny":null,"optionalInt":42}
  ]"""

  val tests = Tests {

    test("default cake") {
      test("serialization with primitives and option") {
        import upickle.implicits.namedTuples.default.given
        val json = upickle.default.write(
          (
            (x = 23, y = 7.5, z = 500_000_000_000L),
            (name = "Alice", isHuman = true, isAlien = false),
            (arr = (1, 2, 3), optionalAny = None, optionalInt = Some(42))
          )
        ) // named tuple write
        assert(json == basic_formatted.replaceAll("\\s+", ""))
      }

      test("deserialization with primitives, seq and option") {
        import upickle.implicits.namedTuples.default.given
        val result = upickle.default.read[
          (
              (x: Int, y: Double, z: Long),
              (name: String, isHuman: Boolean, isAlien: Boolean),
              (
                  arr: Seq[Int],
                  optionalAny: Option[Int],
                  optionalInt: Option[Int]
              )
          )
        ](basic_formatted)

        assert(
          result == (
            (x = 23, y = 7.5, z = 500_000_000_000L),
            (name = "Alice", isHuman = true, isAlien = false),
            (arr = Seq(1, 2, 3), optionalAny = None, optionalInt = Some(42))
          )
        )
      }

      test("readwriter with primitives, seq and option") {
        import upickle.implicits.namedTuples.default.given
        type Schema = (
            (x: Int, y: Double, z: Long),
            (name: String, isHuman: Boolean, isAlien: Boolean),
            (arr: Seq[Int], optionalAny: Option[Int], optionalInt: Option[Int])
        )
        val rw = summon[upickle.default.ReadWriter[Schema]]

        val data: Schema = (
          (x = 23, y = 7.5, z = 500_000_000_000L),
          (name = "Alice", isHuman = true, isAlien = false),
          (arr = Seq(1, 2, 3), optionalAny = None, optionalInt = Some(42))
        )

        val json = upickle.default.write[Schema](data)(using rw)
        assert(json == basic_formatted.replaceAll("\\s+", ""))
        val result = upickle.default.read[Schema](json)(using rw)
        assert(result == data)
      }

      test("unhappy path reading when expected keys are missing") {
        import upickle.implicits.namedTuples.default.given
        val json = """{"bar": 23}"""
        val err = intercept[upickle.core.AbortException] {
          upickle.default.read[(foo: Boolean)](json)
        }
        assert(err.getMessage.contains("missing keys in dictionary: foo"))
      }

      test("unhappy path reading when json is not an object") {
        import upickle.implicits.namedTuples.default.given
        val json = """[]"""
        val err = intercept[upickle.core.AbortException] {
          upickle.default.read[(foo: Boolean)](json)
        }
        assert(err.getMessage.contains("expected dictionary got sequence"))
      }
    }

    test("legacy cake") {

      test("serialization with primitives and option [legacy]") {
        import upickle.implicits.namedTuples.legacy.given
        val json = upickle.legacy.write(
          (
            (x = 23, y = 7.5, z = 500_000_000_000L),
            (name = "Alice", isHuman = true, isAlien = false),
            (arr = (1, 2, 3), optionalAny = None, optionalInt = Some(42))
          )
        )
        assert(json == basic_formatted.replaceAll("\\s+", ""))
      }

      test("deserialization with primitives, seq and option [legacy]") {
        import upickle.implicits.namedTuples.legacy.given
        val result = upickle.legacy.read[
          (
              (x: Int, y: Double, z: Long),
              (name: String, isHuman: Boolean, isAlien: Boolean),
              (
                  arr: Seq[Int],
                  optionalAny: Option[Int],
                  optionalInt: Option[Int]
              )
          )
        ](basic_formatted)

        assert(
          result == (
            (x = 23, y = 7.5, z = 500_000_000_000L),
            (name = "Alice", isHuman = true, isAlien = false),
            (arr = Seq(1, 2, 3), optionalAny = None, optionalInt = Some(42))
          )
        )
      }

      test("readwriter with primitives, seq and option [legacy]") {
        import upickle.implicits.namedTuples.legacy.given
        type Schema = (
            (x: Int, y: Double, z: Long),
            (name: String, isHuman: Boolean, isAlien: Boolean),
            (arr: Seq[Int], optionalAny: Option[Int], optionalInt: Option[Int])
        )
        val rw = summon[upickle.legacy.ReadWriter[Schema]]

        val data: Schema = (
          (x = 23, y = 7.5, z = 500_000_000_000L),
          (name = "Alice", isHuman = true, isAlien = false),
          (arr = Seq(1, 2, 3), optionalAny = None, optionalInt = Some(42))
        )

        val json = upickle.legacy.write[Schema](data)(using rw)
        assert(json == basic_formatted.replaceAll("\\s+", ""))
        val result = upickle.legacy.read[Schema](json)(using rw)
        assert(result == data)
      }
    }

  }
}
