
object Main{
  def main(args: Array[String]): Unit = {
    println(upickle.default.write(Foo(1)))
  }
}
