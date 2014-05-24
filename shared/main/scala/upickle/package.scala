import scala.reflect.ClassTag

/**
 * Picklite tries the following mechanisms for pickling a type
 *
 * - Is there an implicit pickler for that type?
 * - Does the companion have matching apply/unapply?
 * - Does the companion have matching apply/unapplySeq?
 *
 */
package object upickle {
  type PF[A, B] = PartialFunction[A, B]
  def write[T: Writer](expr: T): String = Json.write(writeJs(expr))
  def writeJs[T: Writer](expr: T): Js.Value = implicitly[Writer[T]].write(expr)
  def read[T: Reader](expr: String): T = readJs[T](Json.read(expr))
  def readJs[T: Reader](expr: Js.Value): T = implicitly[Reader[T]].read(expr)

  type CT[V] = ClassTag[V]
  type RW[V] = ReadWriter[V]
  type R[V] = Reader[V]
  type W[V] = Writer[V]
}
