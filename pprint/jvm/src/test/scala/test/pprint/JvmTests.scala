package test.pprint
import utest._
import pprint.Config.Defaults._

// Things we want to test against regressions but only run on the JVM
object JvmTests extends TestSuite{
  implicit val system = akka.actor.ActorSystem()
  val tests = TestSuite{
    'akka {
//      val serverBinding = akka.http.Http(system).bind(interface = "localhost", port = 31337)
//      Check(serverBinding, serverBinding.toString)
//      val materializer = akka.stream.ActorFlowMaterializer()
//      Check(materializer, materializer.toString)


    }

    'spire {


      import spire.implicits._

      import spire.math._

//      def euclidGcd[A: Integral](x: A, y: A): A = {
//        if (y == 0) x
//        else euclidGcd(y, x % y)
//      }
//
//      euclidGcd(42, 96)
//
//      euclidGcd(42L, 96L)
//
//      val e = euclidGcd(BigInt(42), BigInt(96))
//      Check(e, e.toString)
      def mean[A: Fractional](xs: A*): A = xs.reduceLeft(_ + _) / xs.size
//
//      mean(0.5, 1.5, 0.0, -0.5)

      val m = mean(Rational(1, 2), Rational(3, 2), Rational(0))
      implicitly[pprint.PPrint[Rational]]
    }
  }
}
