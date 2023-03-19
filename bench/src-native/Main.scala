package upickle


object Main{
  import ADTs.ADT0
  import Defaults._
  import Generic.ADT
  import Hierarchy._
  import Recursive._
  def main(args: Array[String]): Unit = {
    val allResults = collection.mutable.Buffer.empty[(String, Int)]
    for ((duration, save) <- Seq(2500 -> false, 5000 -> false, 10000 -> true, 10000 -> true, 10000 -> true)) {
      println("RUN NATIVE: " + duration)
      println()

      val results = Seq(
        Common.upickleDefault(duration),
        Common.upickleDefaultByteArray(duration),
        Common.upickleDefaultBinary(duration),

        Common.integers(duration),
        Common.integersByteArray(duration),
        Common.integersBinary(duration),

        Common.doubles(duration),
        Common.doublesByteArray(duration),
        Common.doublesBinary(duration),

        Common.sequences(duration),
        Common.sequencesByteArray(duration),
        Common.sequencesBinary(duration),

        Common.shortStrings(duration),
        Common.shortStringsByteArray(duration),
        Common.shortStringsBinary(duration),

        Common.longStrings(duration),
        Common.longStringsByteArray(duration),
        Common.longStringsBinary(duration),

        Common.unicodeStrings(duration),
        Common.unicodeStringsByteArray(duration),
        Common.unicodeStringsBinary(duration),

        Common.caseClasses(duration),
        Common.caseClassesByteArray(duration),
        Common.caseClassesBinary(duration)
      )
      println()
      if (save) allResults.appendAll(results.flatten)
      println()
    }
    Common.prettyPrintResults(allResults)
  }
}
