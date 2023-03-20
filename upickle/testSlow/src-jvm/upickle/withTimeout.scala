package upickle

import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

object withTimeout {
  def apply(f: => Unit): Unit = {
    Await.result(
      Future {
        blocking { f }
      },
      2.minutes
    )
  }
}
