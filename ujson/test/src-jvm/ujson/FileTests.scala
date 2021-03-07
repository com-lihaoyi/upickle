package ujson

import utest._
import upickle.core.NoOpVisitor

object FileTests extends TestSuite{

  val M = 1
  val q = "\""
  val big = q + ("x" * (40 * M)) + q
  val bigEscaped = q + ("\\\\" * (20 * M)) + q
  def tests = Tests{
    test("large strings in files are ok"){

      TestUtil.withTemp(big) { t =>
        Readable.fromFile(t).transform(NoOpVisitor)
      }

      TestUtil.withTemp(bigEscaped) { t =>
        Readable.fromFile(t).transform(NoOpVisitor)
      }
    }
    test("make sure geny.Readable and InputStreamParser works"){
      TestUtil.withTemp(big) { t =>
        val jsonBytes = java.nio.file.Files.readAllBytes(t.toPath)
        Readable.fromReadable(jsonBytes).transform(NoOpVisitor)
      }

      TestUtil.withTemp(bigEscaped) { t =>
        val jsonBytes = java.nio.file.Files.readAllBytes(t.toPath)
        Readable.fromReadable(jsonBytes).transform(NoOpVisitor)
      }
    }
  }
}
