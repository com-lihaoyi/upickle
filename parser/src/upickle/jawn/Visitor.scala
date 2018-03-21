package upickle.jawn

/**
 * Facade is a type class that describes how Jawn should construct
 * JSON AST elements of type J.
 *
 * Facade[J] also uses FContext[J] instances, so implementors will
 * usually want to define both.
 */
trait Visitor[-T, +J] {
  def singleContext(index: Int): ObjArrVisitor[T, J]
  def arrayContext(index: Int): ArrVisitor[T, J]
  def objectContext(index: Int): ObjVisitor[T, J]

  def jnull(index: Int): J
  def jfalse(index: Int): J
  def jtrue(index: Int): J
  def jnum(s: CharSequence, decIndex: Int, expIndex: Int, index: Int): J
  def jstring(s: CharSequence, index: Int): J


  def singleContext(): ObjArrVisitor[T, J] = singleContext(-1)
  def arrayContext(): ArrVisitor[T, J] = arrayContext(-1)
  def objectContext(): ObjVisitor[T, J] = objectContext(-1)

  def jnull(): J = jnull(-1)
  def jfalse(): J = jfalse(-1)
  def jtrue(): J = jtrue(-1)
  def jnum(s: CharSequence, decIndex: Int, expIndex: Int): J = jnum(s, decIndex, expIndex, -1)
  def jstring(s: CharSequence): J = jstring(s, -1)
}


/**
 * FContext is used to construct nested JSON values.
 *
 * The most common cases are to build objects and arrays. However,
 * this type is also used to build a single top-level JSON element, in
 * cases where the entire JSON document consists of "333.33".
 */
sealed trait ObjArrVisitor[-J, +T] {
  def subVisitor: Visitor[Nothing, Any]

  def add(v: J, index: Int): Unit
  def finish(index: Int): T
  def isObj: Boolean
}
trait ObjVisitor[-J, +T] extends ObjArrVisitor[J, T]{
  def visitKey(s: CharSequence, index: Int): Unit
  def isObj = true
}
trait ArrVisitor[-J, +T] extends ObjArrVisitor[J, T]{
  def isObj = false
}

/**
  * Signals failure processsing JSON after parsing.
  */
case class JsonProcessingException(clue: String,
                                   index: Int,
                                   line: Int,
                                   col: Int,
                                   path: List[Any],
                                   cause: Throwable) extends Exception(clue + " at index " + index, cause)

/**
  * Throw this inside a [[Visitor]]'s handler functions to fail the processing
  * of JSON. The Facade just needs to provide the error message, and it is up
  * to the driver to ensure it is properly wrapped in a [[JsonProcessingException]]
  * with the relevant source information.
  */
case class AbortJsonProcessingException(msg: String) extends Exception(msg)