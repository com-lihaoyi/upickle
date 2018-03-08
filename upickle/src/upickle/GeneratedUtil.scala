package upickle
import jawn.{Facade, RawFContext, RawFacade}

import language.higherKinds
import scala.collection.mutable
/**
* Stuff that generated code depends on but I don't want
* to put in the stringly typed code-generator
*/
private[upickle] trait GeneratedUtil extends Types{
  case class TupleNWriter[V](writers: List[Writer[_]], f: V => Seq[Any]) extends Writer[V]{
    def write(out: jawn.Facade[Unit], v: V) = {
      if (v == null) out.jnull(-1)
      else{

        val ctx = out.arrayContext().asInstanceOf[RawFContext[Unit, Unit]]

        for((item, w) <- f(v).zip(writers)){
          ctx.add((), -1)
          w.asInstanceOf[Writer[Any]].write(out, item)
        }
        ctx.finish(-1)
      }
    }
  }

  case class TupleNReader[V](readers: List[Reader[_]], f: Seq[Any] => V) extends Reader[V]{
    override def arrayContext(index: Int) = new jawn.RawFContext[Any, V] {
      val b = mutable.Buffer.empty[Any]
      var facades = readers

      def visitKey(s: CharSequence, index: Int): Unit = ???

      def add(v: Any, index: Int): Unit = {
        facades = facades.tail
        if (facades.isEmpty) facades = readers
        b += v
      }

      def finish(index: Int) = f(b)

      def isObj = false

      def facade = facades.head.asInstanceOf[jawn.RawFacade[Any]]
    }
  }

  def CaseR[T, V](f: T => V,
                  names: Array[String],
                  defaults: Array[Any])
                 (implicit r: TupleNReader[T]): Reader[V] = new Reader[V]{
    override def objectContext(index: Int) = new RawFContext[Any, V] {
      val b = new Array[Any](names.length)
      var facades = r.readers

      var currentKey: String = null
      def facade = facades.head.asInstanceOf[RawFacade[Any]]

      def visitKey(s: CharSequence, index: Int): Unit = currentKey = s.toString

      def add(v: Any, index: Int): Unit = {
        val i = names.indexOf(currentKey)
        if (i == -1) ???
        else{
          b(i) = v
        }
      }

      def finish(index: Int) = f(r.f(b))

      def isObj = true
    }
  }
  def Case0R[V](f: () => V): Reader[V] = new Reader[V]{
    override def objectContext(index: Int) = new RawFContext[Any, V] {
      def facade = ???

      def visitKey(s: CharSequence, index: Int): Unit = ???

      def add(v: Any, index: Int): Unit = ???

      def finish(index: Int) = f()

      def isObj = true
    }
  }

  def CaseW[T, V](f: V => Option[T],
                  names: Array[String],
                  defaults: Array[Any])
                 (implicit w: TupleNWriter[T]): Writer[V] = new Writer[V]{
    def write(out: Facade[Unit], v: V): Unit = {
      val writers = w.writers.asInstanceOf[Seq[Writer[Any]]]
      val items = w.f(f(v).get)
      for((i, w1) <- items zip writers){
        w1.write(out, i)
      }

    }
  }
  def Case0W[T](f: T => Boolean) = new Writer[T] {
    def write(out: jawn.Facade[Unit], v: T) = out.jnull()
  }
}
