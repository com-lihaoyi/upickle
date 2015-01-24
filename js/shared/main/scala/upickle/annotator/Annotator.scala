package upickle.annotator

import upickle.{Reader, Writer}

import scala.reflect.ClassTag

/**
  * Represents a strategy for storing a type annotation in the serialized data.
  *
  * The strategy wraps the given reader and writer to load and store the type annotation. It is supposed
  * that actual implementations have compatible `annotate` methods.
  */
trait Annotator {
   def annotate[V](r: Reader[V], name: String): Reader[V]
   def annotate[V: ClassTag](w: Writer[V], name: String): Writer[V]
}

object Annotator {
   implicit val defaultAnnotator = array.ArrayAnnotator
}
