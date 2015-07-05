package test.pprint

object Nested{
  object ODef { case class Foo(i: Int, s: String) }

  class CDef { case class Foo(i: Int, s: String) }
  object CDef extends CDef

}
case class Foo(integer: Int, sequence: Seq[String])

case class FooG[T](t: T, sequence: Seq[String])
case class FooNoArgs()