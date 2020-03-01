package ujson

import org.scalatest.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import upickle.core.NoOpVisitor

class ChannelSpec extends AnyPropSpec with Matchers with ScalaCheckPropertyChecks {

  val M = 1
  val q = "\""
  val big = q + ("x" * (40 * M)) + q
  val bigEscaped = q + ("\\\\" * (20 * M)) + q
  property("large strings in files are ok") {

    TestUtil.withTemp(big) { t =>
      scala.util.Try(Readable.fromFile(t).transform(NoOpVisitor)).isSuccess shouldBe true
    }

    TestUtil.withTemp(bigEscaped) { t =>
      scala.util.Try(Readable.fromFile(t).transform(NoOpVisitor)).isSuccess shouldBe true
    }
  }
  property("make sure geny.Readable and InputStreamParser works") {
    TestUtil.withTemp(big) { t =>
      val jsonBytes = java.nio.file.Files.readAllBytes(t.toPath)
      scala.util.Try(Readable.fromReadable(jsonBytes).transform(NoOpVisitor)).isSuccess shouldBe true
    }

    TestUtil.withTemp(bigEscaped) { t =>
      val jsonBytes = java.nio.file.Files.readAllBytes(t.toPath)
      scala.util.Try(Readable.fromReadable(jsonBytes).transform(NoOpVisitor)).isSuccess shouldBe true
    }
  }
}
