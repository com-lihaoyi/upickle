package example

import upickle.TestUtil._
import upickle._
import utest._


object TestUPickle extends App {
  implicit val ww1 = implicitly[upickle.Writer[A1]]
  // kill compiler
  case class A1 (x: A2 , y: A2 )
  case class A2 (x: A3 , y: A3 )
  case class A3 (x: A4 , y: A4 )
  case class A4 (x: A5 , y: A5 )
  case class A5 (x: A6 , y: A6 )
  case class A6 (x: A7 , y: A7 )
  case class A7 (x: A8 , y: A8 )
  case class A8 (x: A9 , y: A9 )
  case class A9 (x: A10, y: A10)
  case class A10(x: A11, y: A11)
  case class A11(x: A12, y: A12)
  case class A12(x: A13, y: A13)
  case class A13(x: A14, y: A14)
  case class A14(x: A15, y: A15)
  case class A15(x: A16, y: A16)
  case class A16(x: A17, y: A17)
  case class A17(x: A18, y: A18)
  case class A18()

}
case class A[T](t: T)
case class Fee(i: Int, s: String)
