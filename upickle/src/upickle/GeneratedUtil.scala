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
  class TupleNWriter[V](val writers: List[Writer[_]], val f: V => Seq[Any]) extends Writer[V]{
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

  class TupleNReader[V](val readers: List[Reader[_]], val f: Seq[Any] => V) extends Reader[V]{
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

  class CaseR[T, V](f: T => V,
                    names: Array[String],
                    defaults: Array[Any],
                    val tags: Seq[String])
                   (implicit r: TupleNReader[T]) extends Reader[V]{
    override def objectContext(index: Int) = new RawFContext[Any, V] {
      val b = new Array[Any](names.length)
      var facades = r.readers

      var currentKey: String = null
      def facade = facades.head.asInstanceOf[RawFacade[Any]]

      def visitKey(s: CharSequence, index: Int): Unit = currentKey = s.toString

      def add(v: Any, index: Int): Unit = {
        facades = facades.tail
        if (facades.isEmpty) facades = r.readers
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
  class Case0R[V](f: () => V, val tags: Seq[String]) extends Reader[V]{ outer =>
    override def objectContext(index: Int) = new RawFContext[Any, V] {
      def facade = outer.asInstanceOf[jawn.RawFacade[Any]]

      def visitKey(s: CharSequence, index: Int): Unit = ???

      def add(v: Any, index: Int): Unit = ???

      def finish(index: Int) = f()

      def isObj = true
    }
  }

  class CaseW[T, V](val f: V => Option[T],
                    val names: Array[String],
                    val defaults: Array[Any],
                    val tags: Seq[String])
                   (implicit val w: TupleNWriter[T]) extends Writer[V]{
    def write(out: Facade[Unit], v: V): Unit = {
      val writers = w.writers.asInstanceOf[Seq[Writer[Any]]]
      val ctx = out.objectContext(-1).asInstanceOf[RawFContext[Any, Any]]
      val items = w.f(f(v).get)
      for(i <- names.indices){
        ctx.add((), -1)
        ctx.visitKey(names(i), -1)
        writers(i).write(out, items(i))
      }
      ctx.finish(-1)
    }
  }
  class Case0W[T](f: T => Boolean, val tags: Seq[String]) extends Writer[T] {
    def write(out: jawn.Facade[Unit], v: T) = {
      out.objectContext(-1).finish(-1)
    }
  }


  def annotate[V: ClassTag](rw: Reader[V], n: String) = new TaggedReader[V] {
    def tags = Seq(n)

    def readers = Seq(rw)
  }

  def annotate[V: ClassTag](rw: Writer[V], n: String) = new TaggedWriter[V]{
    def tags = Seq(n)

    def write(out: Facade[Unit], v: V) = {
      val ctx = out.arrayContext(-1).asInstanceOf[RawFContext[Any, _]]
      ctx.add((), -1)
      out.jstring(n, -1)
      ctx.add((), -1)
      rw.write(out, v)
      ctx.finish(-1)
    }
  }


}
