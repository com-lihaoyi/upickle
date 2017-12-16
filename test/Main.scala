import upickle.default.{macroRW, ReadWriter => RW}

case class Thing(myFieldA: Int, myFieldB: String)
object Thing{
  implicit def rw: RW[Thing] = macroRW
}
object Main{
  def main(args: Array[String]): Unit = {
    println(upickle.default.write(Thing(1, "2")))
  }
}
