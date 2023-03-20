package upickle

object withTimeout {
  // In Scala Native and Scala.js it's impossible to
  // timeout a blocking operation. So we skip the timeout
  def apply(f: => Unit): Unit = {
    f
  }
}
