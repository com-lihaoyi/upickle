package upickle.implicits

import scala.language.experimental.macros
import scala.language.higherKinds
import scala.reflect.ClassTag
import upickle.core.{Visitor, Abort, ArrVisitor, ObjVisitor, NoOpVisitor}

/**
* Basic functionality to be able to read and write objects. Kept as a trait so
* other internal files can use it, while also mixing it into the `upickle`
* package to form the public API1
*/
trait TupleReadWriters extends upickle.core.Types{


  class TupleNWriter[V](val writers: Array[Writer[_]], val f: V => Array[Any]) extends Writer[V]{
    def write0[R](out: Visitor[_, R], v: V): R = {
      if (v == null) out.visitNull(-1)
      else{
        val ctx = out.visitArray(writers.length, -1)
        val vs = f(v)
        var i = 0
        while(i < writers.length){
          ctx.visitValue(
            writers(i).asInstanceOf[Writer[Any]].write(
              ctx.subVisitor.asInstanceOf[Visitor[Any, Nothing]],
              vs(i)
            ),
            -1
          )
          i += 1
        }
        ctx.visitEnd(-1)
      }
    }
  }

  class TupleNReader[V](val readers: Array[Reader[_]], val f: Array[Any] => V) extends SimpleReader[V]{

    override def expectedMsg = "expected sequence"
    override def visitArray(length: Int, index: Int) = new ArrVisitor[Any, V] {
      val b = new Array[Any](readers.length)
      var facadesIndex = 0

      var start = facadesIndex
      def visitValue(v: Any, index: Int): Unit = {
        b(facadesIndex % readers.length) = v
        facadesIndex = facadesIndex + 1
      }

      def visitEnd(index: Int) = {
        val lengthSoFar = facadesIndex - start
        if (lengthSoFar != readers.length) {
          throw new Abort(
            "expected " + readers.length + " items in sequence, found " + lengthSoFar
          )
        }
        start = facadesIndex

        f(b)

      }

      def subVisitor: Visitor[_, _] = {
        readers(facadesIndex % readers.length)
      }
    }
  }

}
