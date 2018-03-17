package upickle
package api

import upickle.jawn.{Facade, AbortJsonProcessingException, RawFContext}

import scala.language.higherKinds

/**
* Stuff that generated code depends on but I don't want
* to put in the stringly typed code-generator
*/
private[upickle] trait GeneratedUtil extends upickle.core.Types{
  class TupleNWriter[V](val writers: Array[Writer[_]], val f: V => Array[Any]) extends Writer[V]{
    def write[R](out: upickle.jawn.Facade[R], v: V): R = {
      if (v == null) out.jnull(-1)
      else{
        val ctx = out.arrayContext()
        val vs = f(v)
        var i = 0
        while(i < writers.length){
          ctx.add(writers(i).asInstanceOf[Writer[Any]].write(out, vs(i)), -1)
          i += 1
        }
        ctx.finish(-1)
      }
    }
  }

  class TupleNReader[V](val readers: Array[Reader[_]], val f: Array[Any] => V) extends Reader[V]{
    override def expectedMsg = "expected sequence"
    override def arrayContext(index: Int) = new upickle.jawn.RawFContext[Any, V] {
      val b = new Array[Any](readers.length)
      var facadesIndex = 0

      var start = facadesIndex
      def visitKey(s: CharSequence, index: Int): Unit = ???

      def add(v: Any, index: Int): Unit = {
        b(facadesIndex % readers.length) = v
        facadesIndex = facadesIndex + 1
      }

      def finish(index: Int) = {
        val lengthSoFar = facadesIndex - start
        if (lengthSoFar != readers.length) {
          throw new AbortJsonProcessingException(
            "expected " + readers.length + " items in sequence, found " + lengthSoFar
          )
        }
        start = facadesIndex
        f(b)

      }

      def isObj = false

      def facade = readers(facadesIndex % readers.length)
    }
  }

  abstract class CaseR[V](val argCount: Int) extends Reader[V]{
    override def expectedMsg = "expected dictionary"
    trait CaseObjectContext extends upickle.jawn.RawFContext[Any, V]{

      val aggregated = new Array[Any](argCount)
      val found = new Array[Boolean](argCount)
      var currentIndex = -1
      var count = 0
      def add(v: Any, index: Int): Unit = {
        if (currentIndex != -1 && !found(currentIndex)) {
          count += 1
          aggregated(currentIndex) = v
          found(currentIndex) = true
        }
      }

      def isObj = true
    }
  }

  class SingletonR[T](t: T) extends Reader[T]{
    override def expectedMsg = "expected dictionary"
    override def objectContext(index: Int) = new RawFContext[Any, T] {
      def facade = upickle.jawn.NullFacade

      def visitKey(s: CharSequence, index: Int): Unit = ???

      def add(v: Any, index: Int): Unit = ???

      def finish(index: Int) = t

      def isObj = true
    }
  }
  class SingletonW[T](f: T) extends Writer[T] {
    def write[R](out: Facade[R], v: T): R = out.objectContext().finish(-1)
  }


}
