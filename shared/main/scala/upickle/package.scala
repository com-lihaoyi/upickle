import scala.reflect.ClassTag
import acyclic.file

/**
 * Picklite tries the following mechanisms for pickling a type
 *
 * - Is there an implicit pickler for that type?
 * - Does the companion have matching apply/unapply?
 *
 *
 */
package object upickle extends Implicits with Generated with Types{

  /**
   * APIs that need to be exposed to the outside world to support Macros
   * which depend on them, but probably should not get used directly.
   */
  object Internal extends InternalUtils with InternalGenerated
}