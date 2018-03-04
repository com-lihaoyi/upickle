package jawn

/**
 * Facade is a type class that describes how Jawn should construct
 * JSON AST elements of type J.
 *
 * Facade[J] also uses FContext[J] instances, so implementors will
 * usually want to define both.
 */
trait Facade[J] extends RawFacade[J]{
  def singleContext(): RawFContext[J]
  def arrayContext(): RawFContext[J]
  def objectContext(): RawFContext[J]

  def jnull(): J
  def jfalse(): J
  def jtrue(): J
  def jnum(s: CharSequence, decIndex: Int, expIndex: Int): J
  def jstring(s: CharSequence): J

  def singleContext(index: Int) = singleContext()
  def arrayContext(index: Int) = arrayContext()
  def objectContext(index: Int) = objectContext()

  def jnull(index: Int) = jnull()
  def jfalse(index: Int) = jfalse()
  def jtrue(index: Int) = jtrue()
  def jnum(s: CharSequence, decIndex: Int, expIndex: Int, index: Int) =
    jnum(s, decIndex, expIndex)
  def jstring(s: CharSequence, index: Int) = jstring(s)
}
/**
 * Facade is a type class that describes how Jawn should construct
 * JSON AST elements of type J.
 *
 * Facade[J] also uses FContext[J] instances, so implementors will
 * usually want to define both.
 */
trait RawFacade[J] {
  def singleContext(index: Int): RawFContext[J]
  def arrayContext(index: Int): RawFContext[J]
  def objectContext(index: Int): RawFContext[J]

  def jnull(index: Int): J
  def jfalse(index: Int): J
  def jtrue(index: Int): J
  def jnum(s: CharSequence, decIndex: Int, expIndex: Int, index: Int): J
  def jstring(s: CharSequence, index: Int): J
}

/**
 * FContext is used to construct nested JSON values.
 *
 * The most common cases are to build objects and arrays. However,
 * this type is also used to build a single top-level JSON element, in
 * cases where the entire JSON document consists of "333.33".
 */
trait FContext[J] extends RawFContext[J]{
  def add(s: CharSequence): Unit
  def add(v: J): Unit
  def finish(): J


  def add(s: CharSequence, index: Int) = add(s)
  def add(v: J, index: Int) = add(v)
  def finish(index: Int) = finish()

  def isObj: Boolean
}

/**
 * FContext is used to construct nested JSON values.
 *
 * The most common cases are to build objects and arrays. However,
 * this type is also used to build a single top-level JSON element, in
 * cases where the entire JSON document consists of "333.33".
 */
trait RawFContext[J] {
  def add(s: CharSequence, index: Int): Unit
  def add(v: J, index: Int): Unit
  def finish(index: Int): J
  def isObj: Boolean
}
