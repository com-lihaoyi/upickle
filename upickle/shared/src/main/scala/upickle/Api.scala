package upickle

/**
 * Created by haoyi on 7/4/15.
 */
class Api extends Types with Implicits with Generated {
  type Writer[T] = upickle.Writer[T]
  val Writer = upickle.Writer
  type Reader[T] = upickle.Reader[T]
  val Reader = upickle.Reader

  protected[this] def validate[T](name: String)(pf: PartialFunction[Js.Value, T]) = Internal.validate(name)(pf)
}

object old extends Api