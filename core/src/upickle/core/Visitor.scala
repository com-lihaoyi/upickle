package upickle.core

/**
  * Standard set of hooks uPickle uses to traverse over a structured data.
  * A superset of the JSON, MessagePack, and Scala object  hierarchies, since
  * it needs to support efficiently processing all of them.
  *
  * Note that some parameters are un-set (-1) when not available; e.g.
  * `visitArray`'s `length` is not set when parsing JSON input (since it cannot
  * be known up front) and the various `index` parameters are not set when
  * traversing Scala object hierarchies.
  *
  * When expecting to deal with a subset of the methods; it is common to
  * forward the ones you don't care about to the ones you do; e.g. JSON visitors
  * would forward all `visitFloat32`/`visitInt`/etc. methods to `visitFloat64`
  *
  * @see [[http://www.lihaoyi.com/post/ZeroOverheadTreeProcessingwiththeVisitorPattern.html]]
  * @tparam T ???
  * @tparam J the result of visiting elements (e.g. a json AST or side-effecting writer)
  */
trait Visitor[-T, +J] {


  /**
    * @param index json source position at the start of the `[` being visited
    * @return a [[Visitor]] used for visiting the elements of the array
    */
  def visitArray(length: Int, index: Int): ArrVisitor[T, J]

  /**
    * @param index json source position at the start of the `{` being visited
    * @return a [[ObjVisitor]] used for visiting the keys/values of the object
    */
  def visitObject(length: Int, index: Int): ObjVisitor[T, J]

  /**
    * @param index json source position at the start of the `null` being visited
    */
  def visitNull(index: Int): J

  /**
    * @param index json source position at the start of the `false` being visited
    */
  def visitFalse(index: Int): J

  /**
    * @param index json source position at the start of the `true` being visited
    */
  def visitTrue(index: Int): J

  /**
    * Visit the number in its text representation.
    *
    * @param s        unparsed text representation of the number.
    * @param decIndex index of the `.`, relative to the start of the CharSequence, or -1 if omitted
    * @param expIndex index of `e` or `E` relative to the start of the CharSequence, or -1 if omitted
    * @param index    json source position at the start of the number being visited
    */
  def visitFloat64StringParts(s: CharSequence, decIndex: Int, expIndex: Int, index: Int): J

  /**
    * Optional handler for raw double values; can be overriden for performance
    * in cases where you're translating directly between numbers to avoid the
    * overhead of stringifying and re-parsing your numbers (e.g. the WebJson
    * transformer gets raw doubles from the underlying Json.parse).
    *
    * Delegates to `visitFloat64StringParts` if not overriden
    *
    * @param d     the input number
    * @param index json source position at the start of the number being visited
    */
  def visitFloat64(d: Double, index: Int): J
  def visitFloat32(d: Float, index: Int): J

  def visitInt32(i: Int, index: Int): J
  def visitInt64(i: Long, index: Int): J
  def visitUInt64(i: Long, index: Int): J

  /**
    * Convenience methods to help you compute the decimal-point-index and
    * exponent-index of an arbitrary numeric string
    *
    * @param s     the text string being visited
    * @param index json source position at the start of the string being visited
    */
  def visitFloat64String(s: String, index: Int): J

  /**
    * @param s     the text string being visited
    * @param index json source position at the start of the string being visited
    */
  def visitString(s: CharSequence, index: Int): J
  def visitChar(s: Char, index: Int): J

  def visitBin(bytes: Array[Byte], offset: Int, len: Int, index: Int): J

  def visitExt(tag: Byte, bytes: Array[Byte], offset: Int, len: Int, index: Int): J
}

/**
  * Base class for visiting elements of json arrays and objects.
  *
  * @tparam T ???
  * @tparam J the result of visiting elements (e.g. a json AST or side-effecting writer)
  */
sealed trait ObjArrVisitor[-T, +J] {

  /**
    * Called on descent into elements.
    *
    * The returned [[Visitor]] will be used to visit this branch of the json.
    */
  def subVisitor: Visitor[Nothing, Any]

  /**
    * Called on completion of visiting an array element or object field value, with the produced result, [[T]].
    *
    * @param v     result of visiting a value in this object or arary
    *              (not the input value, this would have been passed to [[subVisitor]])
    * @param index json source character position being visited
    */
  def visitValue(v: T, index: Int): Unit

  /**
    * Called on end of the object or array.
    *
    * @param index json source position at the start of the '}' or ']' being visited
    * @return the result of visiting this array or object
    */
  def visitEnd(index: Int): J

  /**
    * @return true if this is a json object
    *         false if this is a json array
    */
  def isObj: Boolean

  /**
    * Casts [[T]] from _ to [[Any]].
    */
  def narrow = this.asInstanceOf[ObjArrVisitor[Any, J]]
}

/**
  * Visits the elements of a json object.
  */
trait ObjVisitor[-T, +J] extends ObjArrVisitor[T, J] {

  /**
    * @param s     the value of the key
    * @param index json source position at the start of the key being visited
    */
  def visitKey(index: Int): Visitor[_, _]
  def visitKeyValue(v: Any): Unit
  def isObj = true
  override def narrow = this.asInstanceOf[ObjVisitor[Any, J]]
}

/**
  * Visits the elements of a json array.
  */
trait ArrVisitor[-T, +J] extends ObjArrVisitor[T, J]{
  def isObj = false

  override def narrow = this.asInstanceOf[ArrVisitor[Any, J]]
}

/**
  * Signals failure processsing JSON after parsing.
  */
case class AbortException(clue: String,
                          index: Int,
                          line: Int,
                          col: Int,
                          path: List[Any],
                          cause: Throwable) extends Exception(clue + " at index " + index, cause)

/**
  * Throw this inside a [[Visitor]]'s handler functions to fail the processing
  * of JSON. The Facade just needs to provide the error message, and it is up
  * to the driver to ensure it is properly wrapped in a [[AbortException]]
  * with the relevant source information.
  */
case class Abort(msg: String) extends Exception(msg)
