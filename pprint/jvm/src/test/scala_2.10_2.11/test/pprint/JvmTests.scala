package test.pprint
import utest._
import pprint.Config.Defaults._

// Things we want to test against regressions but only run on the JVM
object JvmTests extends TestSuite{
  implicit val system = akka.actor.ActorSystem()
  val tests = TestSuite{
    'akka {
      val serverBinding = akka.http.Http(system).bind(interface = "localhost", port = 31337)
      Check(serverBinding, serverBinding.toString)
      val materializer = akka.stream.ActorFlowMaterializer()
      Check(materializer, materializer.toString)


    }
    'finagle{
      implicitly[pprint.PPrint[com.twitter.util.Future[Unit]]]
    }
    'spire {

      import spire.implicits._

      import spire.math._

      def mean[A: Fractional](xs: A*): A = xs.reduceLeft(_ + _) / xs.size
//

      val m = mean(Rational(1, 2), Rational(3, 2), Rational(0))
      implicitly[pprint.PPrint[Rational]]

      import spire.implicits._
      import spire.math._
      Check(
        Interval(0, 10),
        "[0, 10]"
      )
    }
    'doobie{
      import scalaz._, Scalaz._
      import doobie.imports._
      Check(
        42.point[ConnectionIO],
        "Return(42)"
      )
    }
    'coproduct{
      import shapeless.{:+:, CNil, Coproduct}
      type X = Int :+: String :+: CNil

      Check(
        Coproduct[X](1),
        "1"
      )
    }
  }
}
