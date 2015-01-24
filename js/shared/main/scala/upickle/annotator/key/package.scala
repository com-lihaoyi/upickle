package upickle.annotator

import upickle.{Reader, Js, Writer}

import scala.reflect.ClassTag

package object key {
  def keyAnnotator(key: String): Annotator = new Annotator {
    override def annotate[V: ClassTag](w: Writer[V], name: String) = Writer[V] {
      case x: V => w.write(x) match {
        case o: Js.Obj => Js.Obj((key -> Js.Str(name)) +: o.value :_*)
        case o => Js.Arr(Js.Str(name), o)
      }
    }

    override def annotate[V](r: Reader[V], name: String) = Reader[V] {
      case o: Js.Obj if o.value.contains(key -> Js.Str(name)) => r.read(o)
      case Js.Arr(Js.Str(`name`), x) => r.read(x)
    }
  }

  implicit val defaultKeyAnnotator = keyAnnotator("$variant")
}
