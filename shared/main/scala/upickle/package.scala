import scala.reflect.ClassTag
import acyclic.file

/**
 * Picklite tries the following mechanisms for pickling a type
 *
 * - Is there an implicit pickler for that type?
 * - Does the companion have matching apply/unapply?
 * - Does the companion have matching apply/unapplySeq?
 *
 */
package object upickle extends Implicits with Generated{
  object Internal extends InternalCases with InternalThings
}