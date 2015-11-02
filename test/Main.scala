case class A_(objects: List[C_]); case class C_(nodes: List[C_])

object Main{
  def main(args: Array[String]): Unit = {
    upickle.default.write(A_(List(C_(Nil))))
  }
}