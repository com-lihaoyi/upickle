import scala.reflect.ClassTag
import acyclic.file

/**
 * Picklite tries the following mechanisms for pickling a type
 *
 * - Is there an implicit pickler for that type?
 * - Does the companion have matching apply/unapply?
 */
package object upickle extends Types with Implicits with Generated {
  protected[this] def validate[T](name: String)(pf: PartialFunction[Js.Value, T]) = Internal.validate(name)(pf)
}