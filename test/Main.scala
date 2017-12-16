import upickle.default
import upickle.default.{macroRW, ReadWriter => RW, Reader => R, Writer => W}


object Defaults {
  case class ADTa(i: Int = 0)
  object ADTa{
    implicit def rw: RW[ADTa] = default.macroRW
  }
}

object Main{
  def main(args: Array[String]): Unit = {
    println(upickle.default.write(Defaults.ADTa()))
  }
}
