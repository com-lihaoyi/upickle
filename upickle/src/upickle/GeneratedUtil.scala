package upickle
import jawn.RawFContext

import language.higherKinds
import scala.collection.mutable
/**
* Stuff that generated code depends on but I don't want
* to put in the stringly typed code-generator
*/
private[upickle] trait GeneratedUtil extends Types{
  def TupleNWriter[V](writers: List[Writer[_]], f: V => Seq[Any]) = new Writer[V]{
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

  def TupleNReader[V](readers: List[Reader[_]], f: Seq[Any] => V) = new Reader[V]{
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
}
