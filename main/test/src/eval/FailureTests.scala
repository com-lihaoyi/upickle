package mill.eval
import mill.T
import mill.util.{TestEvaluator, TestUtil}
import mill.api.Result.OuterStack
import utest._
import utest.framework.TestPath


object FailureTests extends TestSuite{

  val tests = Tests{
    val graphs = new mill.util.TestGraphs()
    import graphs._

    'evaluateSingle - {
      val check = new TestEvaluator(singleton)
      check.fail(
        target = singleton.single,
        expectedFailCount = 0,
        expectedRawValues = Seq(Result.Success(0))
      )

      singleton.single.failure = Some("lols")

      check.fail(
        target = singleton.single,
        expectedFailCount = 1,
        expectedRawValues = Seq(Result.Failure("lols"))
      )

      singleton.single.failure = None

      check.fail(
        target = singleton.single,
        expectedFailCount = 0,
        expectedRawValues = Seq(Result.Success(0))
      )


      val ex = new IndexOutOfBoundsException()
      singleton.single.exception = Some(ex)


      check.fail(
        target = singleton.single,
        expectedFailCount = 1,
        expectedRawValues = Seq(Result.Exception(ex, new OuterStack(Nil)))
      )
    }
    'evaluatePair - {
      val check = new TestEvaluator(pair)
      check.fail(
        pair.down,
        expectedFailCount = 0,
        expectedRawValues = Seq(Result.Success(0))
      )

      pair.up.failure = Some("lols")

      check.fail(
        pair.down,
        expectedFailCount = 1,
        expectedRawValues = Seq(Result.Skipped)
      )

      pair.up.failure = None

      check.fail(
        pair.down,
        expectedFailCount = 0,
        expectedRawValues = Seq(Result.Success(0))
      )

      pair.up.exception = Some(new IndexOutOfBoundsException())

      check.fail(
        pair.down,
        expectedFailCount = 1,
        expectedRawValues = Seq(Result.Skipped)
      )
    }

    "evaluatePair (failFast=true)"- {
      val check = new TestEvaluator(pair, failFast = true)
      check.fail(
        pair.down,
        expectedFailCount = 0,
        expectedRawValues = Seq(Result.Success(0))
      )

      pair.up.failure = Some("lols")

      check.fail(
        pair.down,
        expectedFailCount = 1,
        expectedRawValues = Seq(Result.Aborted)
      )

      pair.up.failure = None

      check.fail(
        pair.down,
        expectedFailCount = 0,
        expectedRawValues = Seq(Result.Success(0))
      )

      pair.up.exception = Some(new IndexOutOfBoundsException())

      check.fail(
        pair.down,
        expectedFailCount = 1,
        expectedRawValues = Seq(Result.Aborted)
      )
    }

    'evaluateBacktickIdentifiers - {
      val check = new TestEvaluator(bactickIdentifiers)
      import bactickIdentifiers._
      check.fail(
        `a-down-target`,
        expectedFailCount = 0,
        expectedRawValues = Seq(Result.Success(0))
      )

      `up-target`.failure = Some("lols")

      check.fail(
        `a-down-target`,
        expectedFailCount = 1,
        expectedRawValues = Seq(Result.Skipped)
      )

      `up-target`.failure = None

      check.fail(
        `a-down-target`,
        expectedFailCount = 0,
        expectedRawValues = Seq(Result.Success(0))
      )

      `up-target`.exception = Some(new IndexOutOfBoundsException())

      check.fail(
        `a-down-target`,
        expectedFailCount = 1,
        expectedRawValues = Seq(Result.Skipped)
      )
    }

    "evaluateBacktickIdentifiers (failFast=true)" - {
      val check = new TestEvaluator(bactickIdentifiers, failFast = true)
      import bactickIdentifiers._
      check.fail(
        `a-down-target`,
        expectedFailCount = 0,
        expectedRawValues = Seq(Result.Success(0))
      )

      `up-target`.failure = Some("lols")

      check.fail(
        `a-down-target`,
        expectedFailCount = 1,
        expectedRawValues = Seq(Result.Aborted)
      )

      `up-target`.failure = None

      check.fail(
        `a-down-target`,
        expectedFailCount = 0,
        expectedRawValues = Seq(Result.Success(0))
      )

      `up-target`.exception = Some(new IndexOutOfBoundsException())

      check.fail(
        `a-down-target`,
        expectedFailCount = 1,
        expectedRawValues = Seq(Result.Aborted)
      )
    }

    'multipleUsesOfDest - {
      object build extends TestUtil.BaseModule {
        // Using `T.ctx(  ).dest` twice in a single task is ok
        def left = T{ + T.dest.toString.length + T.dest.toString.length }

        // Using `T.ctx(  ).dest` once in two different tasks is not ok
        val task = T.task{ T.dest.toString.length  }
        def right = T{ task() + left() + T.dest.toString().length }
      }

      val check = new TestEvaluator(build)
      val Right(_) = check(build.left)
      val Left(Result.Exception(e, _)) = check(build.right)
      assert(e.getMessage.contains("`dest` can only be used in one place"))
    }
  }
}
