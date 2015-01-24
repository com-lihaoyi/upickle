package upickle.annotator

import upickle.{Reader, Js, Writer}

import scala.reflect.ClassTag

package object array {
  implicit object ArrayAnnotator extends Annotator {
    override def annotate[V: ClassTag](w: Writer[V], name: String) = Writer[V] {
      case x: V => Js.Arr(Js.Str(name), w.write(x))
    }

    override def annotate[V](r: Reader[V], name: String) = Reader[V] {
      case Js.Arr(Js.Str(`name`), x) => r.read(x)
    }
  }
}
