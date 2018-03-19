package upickle

object Main{
  def main(args: Array[String]): Unit = {
    for(duration <- Seq(5000, 50000)){
      println("RUN JS: " + duration)
      println()
      Common.playJson(duration)
      Common.circe(duration)
      Common.upickleDefault(duration)
      Common.upickleLegacy(duration)
      Common.playJsonCached(duration)
      Common.circeCached(duration)
      Common.upickleDefaultCached(duration)
      Common.upickleLegacyCached(duration)
      println()
    }
  }
}
