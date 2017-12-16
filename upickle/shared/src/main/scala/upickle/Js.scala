package upickle

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
    extends scala.Exception(s"$msg (input: $input)")
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
    extends Exception(s"$msg (data: $data)")
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

    /**
      * Returns the `String` value of this [[Js.Value]], fails if it is not
      * a [[Js.Str]]
      */
    def str = this match{
      case Str(value) => value
      case _ => throw Invalid.Data(this, "Expected Js.Str")
    }
    /**
      * Returns the key/value map of this [[Js.Value]], fails if it is not
      * a [[Js.Obj]]
      */
    def obj = this match{
      case Obj(value @_*) => value.toMap
      case _ => throw Invalid.Data(this, "Expected Js.Obj")
    }
    /**
      * Returns the elements of this [[Js.Value]], fails if it is not
      * a [[Js.Arr]]
      */
    def arr = this match{
      case Arr(value @ _*) => value
      case _ => throw Invalid.Data(this, "Expected Js.Arr")
    }
    /**
      * Returns the `Double` value of this [[Js.Value]], fails if it is not
      * a [[Js.Num]]
      */
    def num = this match{
      case Num(value) => value
      case _ => throw Invalid.Data(this, "Expected Js.Num")
    }

    /**
      * Looks up the [[Js.Value]] as a [[Js.Arr]] using an index, throws
      * otherwise if it's not a [[Js.Arr]]
      */
    def apply(i: Int): Value = this.arr(i)
    /**
      * Looks up the [[Js.Value]] as a [[Js.Obj]] using an index, throws
      * otherwise if it's not a [[Js.Obj]]
      */
    def apply(s: java.lang.String): Value = this.obj(s)

    override def toString = upickle.json.write(this, indent = 4)
  }
  case class Str(value0: java.lang.CharSequence) extends Value{
    lazy val value: String = value0.toString
  }
  case class Obj(value0: (java.lang.CharSequence, Value)*) extends Value{
    lazy val value: Seq[(String, Value)] = value0.map{case (k, v) => (k.toString, v)}
  }
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

