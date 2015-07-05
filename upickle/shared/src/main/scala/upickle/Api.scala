package upickle

/**
* Created by haoyi on 7/4/15.
*/
class Api extends Types with Implicits with Generated with MacroImplicits{
  protected[this] def validate[T](name: String)(pf: PartialFunction[Js.Value, T]) = Internal.validate(name)(pf)
  type key = derive.key
}

object old extends Api