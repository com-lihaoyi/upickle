package upickle.implicits

import upickle.core.ObjVisitor
trait BaseCaseObjectContext2 extends BaseCaseObjectContext0 {
  var currentIndex = -1
}

@deprecated("Use BaseCaseObjectContext2", "4.0.0")
trait BaseCaseObjectContext extends BaseCaseObjectContext0 {
  override def visitKey(index: Int): _root_.upickle.core.StringVisitor.type = _root_.upickle.core.StringVisitor
  var currentIndex = -1
}
trait BaseCaseObjectContext0 {
  def storeAggregatedValue(currentIndex: Int, v: Any): Unit

  def visitKey(index: Int): _root_.upickle.core.Visitor[_, _] = _root_.upickle.core.StringVisitor

  def storeValueIfNotFound(i: Int, v: Any): Unit

  protected def errorMissingKeys(rawArgsLength: Int, mappedArgs: Array[String]): Unit

  protected def checkErrorMissingKeys(rawArgsBitset: Long): Boolean
}


abstract class CaseObjectContext2[V](fieldCount: Int) extends CaseObjectContext0[V](fieldCount) with BaseCaseObjectContext2 {
  var found = 0L
}

@deprecated("Use CaseObjectContext2", "4.0.0")
abstract class CaseObjectContext[V](fieldCount: Int) extends CaseObjectContext0[V](fieldCount) with BaseCaseObjectContext {
  var found = 0L

}
abstract class CaseObjectContext0[V](fieldCount: Int) extends ObjVisitor[Any, V] with BaseCaseObjectContext0{
  def found: Long
  def found_=(n: Long): Unit
  def currentIndex: Int
  def currentIndex_=(v: Int): Unit

  def visitValue(v: Any, index: Int): Unit = {
    if ((currentIndex != -1) && ((found & (1L << currentIndex)) == 0)) {
      storeAggregatedValue(currentIndex, v)
      found |= (1L << currentIndex)
    }
  }

  def storeValueIfNotFound(i: Int, v: Any) = {
    if ((found & (1L << i)) == 0) {
      found |= (1L << i)
      storeAggregatedValue(i, v)
    }
  }

  protected def errorMissingKeys(rawArgsLength: Int, mappedArgs: Array[String]) = {
    val keys = for {
      i <- 0 until rawArgsLength
      if (found & (1L << i)) == 0
    } yield mappedArgs(i)
    throw new _root_.upickle.core.Abort(
      "missing keys in dictionary: " + keys.mkString(", ")
    )
  }

  protected def checkErrorMissingKeys(rawArgsBitset: Long) = {
    found != rawArgsBitset
  }
}




abstract class HugeCaseObjectContext2[V](fieldCount: Int) extends HugeCaseObjectContext0[V](fieldCount) with BaseCaseObjectContext2 {
  val found = new Array[Long](fieldCount / 64 + 1)
}


@deprecated("Use HugeCaseObjectContext2", "4.0.0")
abstract class HugeCaseObjectContext[V](fieldCount: Int) extends HugeCaseObjectContext0[V](fieldCount) with BaseCaseObjectContext{
  var found = new Array[Long](fieldCount / 64 + 1)
}

abstract class HugeCaseObjectContext0[V](fieldCount: Int) extends ObjVisitor[Any, V] with BaseCaseObjectContext0 {
  def found: Array[Long]
  def currentIndex: Int
  def currentIndex_=(v: Int): Unit

  def visitValue(v: Any, index: Int): Unit = {
    if ((currentIndex != -1) && ((found(currentIndex / 64) & (1L << currentIndex)) == 0)) {
      storeAggregatedValue(currentIndex, v)
      found(currentIndex / 64) |= (1L << currentIndex)
    }
  }

  def storeValueIfNotFound(i: Int, v: Any) = {
    if ((found(i / 64) & (1L << i)) == 0) {
      found(i / 64) |= (1L << i)
      storeAggregatedValue(i, v)
    }
  }

  protected def errorMissingKeys(rawArgsLength: Int, mappedArgs: Array[String]) = {
    val keys = for {
      i <- 0 until rawArgsLength
      if (found(i / 64) & (1L << i)) == 0
    } yield mappedArgs(i)
    throw new _root_.upickle.core.Abort(
      "missing keys in dictionary: " + keys.mkString(", ")
    )
  }

  protected def checkErrorMissingKeys(rawArgsLength: Long) = {
    var bits = 0
    for (v <- found) bits += java.lang.Long.bitCount(v)
    bits != rawArgsLength
  }
}
