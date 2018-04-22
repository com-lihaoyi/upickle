package ujson

/**
 * Facade is a type class that describes how Jawn should construct
 * JSON AST elements of type J.
 *
 * Facade[J] also uses FContext[J] instances, so implementors will
 * usually want to define both.
 */
trait Visitor[-T, +J] {
  def apply(t: Transformable) = t.transform(this)
  def visitArray(index: Int): ArrVisitor[T, J]
  def visitObject(index: Int): ObjVisitor[T, J]

  def visitNull(index: Int): J
  def visitFalse(index: Int): J
  def visitTrue(index: Int): J
  def visitNum(s: CharSequence, decIndex: Int, expIndex: Int, index: Int): J

  /**
    * Optional handler for raw double values; can be overriden for performance
    * in cases where you're translating directly between numbers to avoid the
    * overhead of stringifying and re-parsing your numbers (e.g. the WebJson
    * transformer gets raw doubles from the underlying Json.parse).
    *
    * Delegates to `visitNum` if not overriden
    */
  def visitNumRaw(d: Double, index: Int): J = {
    val i = d.toInt
    if(i == d) visitNum(i.toString, -1, -1, index)
    else visitNumRawString(d.toString, index)
  }

  /**
    * Convenience methods to help you compute the decimal-point-index and
    * exponent-index of an arbitrary numeric string
    */
  def visitNumRawString(s: String, index: Int): J = {
    visitNum(s, s.indexOf('.'), s.indexOf('E') match{case -1 => s.indexOf('e') case n => n}, -1)
  }

  def visitString(s: CharSequence, index: Int): J


  def visitArray(): ArrVisitor[T, J] = visitArray(-1)
  def visitObject(): ObjVisitor[T, J] = visitObject(-1)

  def visitNull(): J = visitNull(-1)
  def visitFalse(): J = visitFalse(-1)
  def visitTrue(): J = visitTrue(-1)
  def visitNum(s: CharSequence, decIndex: Int, expIndex: Int): J = visitNum(s, decIndex, expIndex, -1)
  def visitString(s: CharSequence): J = visitString(s, -1)
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

  def visitValue(v: J, index: Int): Unit
  def visitEnd(index: Int): T
  def isObj: Boolean
  def narrow = this.asInstanceOf[ObjArrVisitor[Any, T]]

}
trait ObjVisitor[-J, +T] extends ObjArrVisitor[J, T] {
  def visitKey(s: CharSequence, index: Int): Unit
  def isObj = true
  override def narrow = this.asInstanceOf[ObjVisitor[Any, T]]
}

trait ArrVisitor[-J, +T] extends ObjArrVisitor[J, T] {
  def isObj = false
  override def narrow = this.asInstanceOf[ArrVisitor[Any, T]]
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