package upickle


object Main{
  import ADTs.ADT0
  import Defaults._
  import Generic.ADT
  import Hierarchy._
  import Recursive._
  def main(args: Array[String]): Unit = {
    for(duration <- Seq(2500, 5000, 10000, 10000, 10000, 10000)){
      println("RUN NATIVE: " + duration)
      println()

      Common.upickleDefault(duration)
      Common.upickleDefaultByteArray(duration)
      Common.pickleDefaultBinary(duration)

      Common.upickleDefaultCached(duration)
      Common.upickleDefaultByteArrayCached(duration)
      Common.upickleDefaultBinaryCached(duration)
      println()
    }
  }
}
