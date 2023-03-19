package upickle

import utest._
import upickle.default.{ReadWriter, macroRW}
object SlowTests extends TestSuite {
  val tests = Tests {
    test("hash-collision") {
      def zeroHashCodeStrings: Iterator[String] = {
        def charAndHash(h: Int): Iterator[(Char, Int)] = ('!' to '~').iterator.map(ch => (ch, (h + ch) * 31))

        for {
          (ch0, h0) <- charAndHash(0)
          (ch1, h1) <- charAndHash(h0)
          (ch2, h2) <- charAndHash(h1) if ((h2 + 32) * 923521 ^ (h2 + 127) * 923521) < 0
          (ch3, h3) <- charAndHash(h2) if ((h3 + 32) * 29791 ^ (h3 + 127) * 29791) < 0
          (ch4, h4) <- charAndHash(h3) if ((h4 + 32) * 961 ^ (h4 + 127) * 961) < 0
          (ch5, h5) <- charAndHash(h4) if ((h5 + 32) * 31 ^ (h5 + 127) * 31) < 0
          (ch6, h6) <- charAndHash(h5) if (h6 + 32 ^ h6 + 127) < 0
          (ch7, _) <- charAndHash(h6) if h6 + ch7 == 0
        } yield new String(Array(ch0, ch1, ch2, ch3, ch4, ch5, ch6, ch7))
      }

      val jsonString =
        zeroHashCodeStrings
          .map(s => ujson.write(s))
          .take(1000000)
          .mkString("{", s":null,", ":null}")

      test("issue-416") {
        withTimeout {
          upickle.default.read[Foo416](jsonString)
        }
      }
      test("issue-446") {
        sys.props("java.vm.name") match {
          case "Scala.js" | "Scala Native" =>
          // The fix assumes a hash collision safe java.util.LinkedHashMap
          // implementation. When/if other platforms will have such characteristics
          // ujson doesn't need to change.
          case _ =>
            withTimeout {
              ujson.read(jsonString)
            }
        }
        ()
      }
    }
    test("fuzz") {
      test("string") {
        test("ascii") - {
          for (i <- Range(0, 4096)) {
            TestUtil.rw("x" * i, "\"" + ("x" * i) + "\"", upack.Str("x" * i))
          }
        }
        test("escaped") - {
          for (i <- Range(0, 4096)) {
            TestUtil.rw("\"" * i, "\"" + ("\\\"" * i) + "\"", upack.Str("\"" * i))
          }
        }
        test("unicode") - {
          for (i <- Range(0, 4096)) {
            TestUtil.rw("包" * i, "\"" + ("包" * i) + "\"", upack.Str("包" * i))
          }
        }
        test("asciiUnicode") - {
          for (i <- Range(0, 4096)) {
            TestUtil.rw("x包" * i, "\"" + "x包" * i + "\"", upack.Str("x包" * i))
          }
        }
        test("unicodeEscaped") - {
          for (i <- Range(0, 4096)) {
            TestUtil.rw("包\"" * i, "\"" + "包\\\"" * i + "\"", upack.Str("包\"" * i))
          }
        }
        test("escapedAscii") - {
          for (i <- Range(0, 4192)) {
            val s =
            TestUtil.rw("\"x" * i, "\"" + "\\\"x" * i + "\"", upack.Str("\"x" * i))
          }
        }
      }
      test("int"){
        test("topLevel"){
          for (i <- Range(Int.MinValue, Int.MaxValue, 13373)) {
            TestUtil.rw(i, s"$i", upack.Int32(i))
          }
        }
        test("nested"){
          for (i <- Range(Int.MinValue, Int.MaxValue, 13373)) {
            TestUtil.rw(Seq(i), s"[$i]", upack.Arr(upack.Int32(i)))
          }
        }
      }
      test("long"){
        test("topLevel"){
          for (i <- Range.Long(Long.MinValue, Long.MaxValue, 337133713371337L)) {
            TestUtil.rw(
              i,
              if (math.abs(i) > math.pow(2, 53) || i == Long.MinValue) "\"" + i + "\"" else s"$i"
            )
          }
        }
        test("nested"){
          for (i <- Range.Long(Long.MinValue, Long.MaxValue, 337133713371337L)) {
            TestUtil.rw(
              Seq(i),
              if (math.abs(i) > math.pow(2, 53) || i == Long.MinValue) "[\"" + i + "\"]" else s"[$i]"
            )
          }
        }
      }
    }
  }
}


case class Foo416()

object Foo416 {
  implicit val rw: ReadWriter[Foo416] = macroRW[Foo416]
}
