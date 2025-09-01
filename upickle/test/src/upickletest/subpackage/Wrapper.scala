package upickletest.subpackage

sealed trait Base
object Base {
  case object Child extends Base
}
case class Wrapper(base: Base)
