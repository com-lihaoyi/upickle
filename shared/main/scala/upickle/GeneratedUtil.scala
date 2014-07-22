package upickle
import acyclic.file
import collection.mutable
/**
 * Stuff that generated code depends on but I don't want
 * to put in the stringly typed code-generator
 */
private[upickle] trait GeneratedUtil {

  def validate[T](name: String)(pf: PartialFunction[Js.Value, T]): PartialFunction[Js.Value, T]

  protected[this] def readerCaseFunction[T](names: Array[String],
                                            defaults: Array[Js.Value],
                                            read: PartialFunction[Js.Value, T]): PartialFunction[Js.Value, T] = {
    validate("Object"){case x: Js.Object => read(mapToArray(x, names, defaults))}
  }
  protected[this] def arrayToMap(a: Js.Array, names: Array[String], defaults: Array[Js.Value]) = {
    Js.Object {
      val accumulated = mutable.Buffer.empty[(String, Js.Value)]
      var i = 0
      val l = a.value.length
      while(i < l){
        if (defaults(i) != a.value(i)){
          accumulated += (names(i) -> a.value(i))
        }
        i += 1
      }
      accumulated
    }
  }
  protected[this] def mapToArray(o: Js.Object, names: Array[String], defaults: Array[Js.Value]) = {
    Js.Array {
      val accumulated = mutable.Buffer.empty[Js.Value]
      val map = o.value.toMap
      var i = 0
      val l = names.length
      while(i < l){
        if (map.contains(names(i))) accumulated += map(names(i))
        else if (defaults(i) != null) accumulated += defaults(i)
        else throw new Invalid.Data(o, "Key Missing: " + names(i))

        i += 1
      }
      accumulated
    }
  }
  protected[this] def RCase[T](names: Array[String],
                             defaults: Array[Js.Value],
                             read: PartialFunction[Js.Value, T]) = {
    Reader[T](readerCaseFunction(names, defaults, read))
  }

  protected[this] def WCase[T](names: Array[String],
                               defaults: Array[Js.Value],
                               write: T => Js.Value) = {
    Writer[T](x => arrayToMap(write(x).asInstanceOf[Js.Array], names, defaults))
  }
}
