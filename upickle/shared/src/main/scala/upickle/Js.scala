package upickle
import acyclic.file
import scala.annotation.switch
import scala.collection.mutable.StringBuilder

/**
 * Exceptions that can be thrown by upickle; placed in the same file
 * as JSON parser due to circular dependencies between exception types
 * and JSON types
 */
sealed trait Invalid extends Exception
object Invalid{

  /**
   * Thrown when the JSON parser finds itself trying to parse invalid JSON.
   * 
   * @param msg Human-readable text saying what went wrong
   * @param input The `String` it was trying to parse
   */
  case class Json(msg: String, input: String)
    extends scala.Exception(s"JsonParse Error: $msg in $input")
    with Invalid

  /**
   * Thrown when uPickle tries to convert a JSON blob into a given data 
   * structure but fails because part the blob is invalid 
   * 
   * @param data The section of the JSON blob that uPickle tried to convert.
   *             This could be the entire blob, or it could be some subtree.
   * @param msg Human-readable text saying what went wrong
   */
  case class Data(data: Js.Value, msg: String)
    extends Exception(s"data: $data msg: $msg")
    with Invalid
}

/**
 * A very small, very simple JSON AST that uPickle uses as part of its
 * serialization process. A common standard between the Jawn AST (which
 * we don't use so we don't pull in the bulk of Spire) and the Javascript
 * JSON AST.
 */
object Js {
  sealed trait Value extends Any {
    def value: Any
    def apply(i: Int): Value = this.asInstanceOf[Arr].value(i)
    def apply(s: java.lang.String): Value = this.asInstanceOf[Obj].value.find(_._1 == s).get._2
  }
  case class Str(value: java.lang.String) extends AnyVal with Value
  case class Obj(value: (java.lang.String, Value)*) extends AnyVal with Value
  case class Arr(value: Value*) extends AnyVal with Value
  case class Num(value: Double) extends AnyVal with Value
  case object False extends Value{
    def value = false
  }
  case object True extends Value{
    def value = true
  }
  case object Null extends Value{
    def value = null
  }
}

