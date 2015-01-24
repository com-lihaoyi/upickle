package upickle

import scala.annotation.implicitNotFound
import scala.reflect.ClassTag

/**
 * Represents a strategy for storing a type annotation in the serialized data.
 *
 * The strategy wraps the given reader and writer to load and store the type annotation. It is supposed
 * that actual implementations have compatible `annotate` methods.
 */
@implicitNotFound("Couldn't find any Annotator implementation in scope. Make sure you have imported some configuration, e.g. `import upickle.config.default._`")
trait Annotator {
  def annotate[V](r: Reader[V], name: String): Reader[V]
  def annotate[V: ClassTag](w: Writer[V], name: String): Writer[V]
}

object Annotator {
  // It drops down to array annotations if the serialized object is not represented as a `Js.Obj`
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

  val arrayAnnotator: Annotator = new Annotator {
    override def annotate[V: ClassTag](w: Writer[V], name: String) = Writer[V] {
      case x: V => Js.Arr(Js.Str(name), w.write(x))
    }

    override def annotate[V](r: Reader[V], name: String) = Reader[V] {
      case Js.Arr(Js.Str(`name`), x) => r.read(x)
    }
  }
}
