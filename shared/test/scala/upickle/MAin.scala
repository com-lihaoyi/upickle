package upickle

/**
 * Created by haoyi on 5/20/14.
 */
object Main {
  def main(args: Array[String]): Unit = {
    sealed trait ConsList
    case class Cons(i: Int, next: ConsList) extends ConsList
    case object End extends ConsList

    implicit lazy val (cLRW, cRW, eRW) = rec{implicit i: ConsList => makeRW[ConsList].make(
      Case2ReadWriter(Cons.apply, Cons.unapply),
      Case0ReadWriter(End)
    )}
    val c = Cons(5, Cons(6, End))
    val cl: ConsList = c
    val serialized = "[0, [5, [0, [6, [1]]]]]"
    Seq(
      write(c) == serialized,
      c == read[ConsList](serialized),
      c == read[Cons](serialized),
      write(cl) == serialized,
      cl == read[ConsList](serialized),
      cl == read[Cons](serialized)
    ).foreach(x => assert(x))
  }
}
