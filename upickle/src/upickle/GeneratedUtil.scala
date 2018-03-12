package upickle
import jawn.{Facade, RawFContext, RawFacade}

import language.higherKinds
import scala.collection.mutable
import scala.reflect.ClassTag
/**
* Stuff that generated code depends on but I don't want
* to put in the stringly typed code-generator
*/
private[upickle] trait GeneratedUtil extends Types{
  class TupleNWriter[V](val writers: Array[Writer[_]], val f: V => Array[Any]) extends Writer[V]{
    def write[R](out: jawn.Facade[R], v: V): R = {
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
    override def arrayContext(index: Int) = new jawn.RawFContext[Any, V] {
      val b = new Array[Any](readers.length)
      var facadesIndex = 0

      def visitKey(s: CharSequence, index: Int): Unit = ???

      def add(v: Any, index: Int): Unit = {
        b(facadesIndex) = v
        facadesIndex = (facadesIndex + 1) % readers.length
      }

      def finish(index: Int) = f(b)

      def isObj = false

      def facade = readers(facadesIndex)
    }
  }

  abstract class CaseR[V](argCount: Int) extends Reader[V]{
    trait CaseObjectContext extends jawn.RawFContext[Any, V]{

      val aggregated = new Array[Any](argCount)
      val found = new Array[Boolean](argCount)
      var currentIndex = -1
      def add(v: Any, index: Int): Unit = {
        if (currentIndex != -1) {
          aggregated(currentIndex) = v
          found(currentIndex) = true
        }
      }

      def isObj = true
    }
  }

  class Case0R[V](f: () => V) extends Reader[V]{ outer =>
    override def objectContext(index: Int) = new RawFContext[Any, V] {
      def facade = outer

      def visitKey(s: CharSequence, index: Int): Unit = ???

      def add(v: Any, index: Int): Unit = ???

      def finish(index: Int) = f()

      def isObj = true
    }
  }

  class Case0W[T](f: T => Boolean) extends Writer[T] {
    def write[R](out: jawn.Facade[R], v: T): R = {
      out.objectContext(-1).finish(-1)
    }
  }

  class SingletonR[T](t: T) extends Reader[T]{
    override def objectContext(index: Int) = new RawFContext[Any, T] {
      def facade = jawn.NullFacade

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
