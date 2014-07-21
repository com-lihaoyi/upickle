package upickle
import acyclic.file
/**
 * Stuff that generated code depends on but I don't want
 * to put in the stringly typed code-generator
 */
private[upickle] trait GeneratedUtil {
  protected[this] def readerCaseFunction[T](names: Seq[String],
                                            defaults: Seq[Js.Value],
                                            read: PartialFunction[Js.Value, T]): PartialFunction[Js.Value, T] = {
    case x: Js.Object => read(mapToArray(x, names, defaults))
  }
  protected[this] def arrayToMap(a: Js.Array, names: Seq[String], defaults: Seq[Js.Value]) = {
    Js.Object(
      names.zip(a.value)
           .zip(defaults)
           .collect{case ((n, v), d) if d != v =>
        (n, v)
      }
    )
  }
  protected[this] def mapToArray(o: Js.Object, names: Seq[String], defaults: Seq[Js.Value]) = {
    Js.Array(
      names.zip(defaults).map{ case (n, d) =>
        o.value
         .toMap
         .get(n)
         .orElse(Option(d))
         .getOrElse{
          throw new Invalid.Data(o, "Key Missing: " + n)
        }
      }
    )
  }
  protected[this] def RCase[T](names: Seq[String],
                             defaults: Seq[Js.Value],
                             read: PartialFunction[Js.Value, T]) = {
    Reader[T](readerCaseFunction(names, defaults, read))
  }

  protected[this] def WCase[T](names: Seq[String],
                               defaults: Seq[Js.Value],
                               write: T => Js.Value) = {
    Writer[T](x => arrayToMap(write(x).asInstanceOf[Js.Array], names, defaults))
  }
}
