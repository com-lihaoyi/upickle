package upickle
import acyclic.file
import collection.mutable
/**
 * Stuff that generated code depends on but I don't want
 * to put in the stringly typed code-generator
 */
private[upickle] trait GeneratedUtil {

  protected[this] def validate[T](name: String)(pf: PartialFunction[Js.Value, T]): PartialFunction[Js.Value, T]

  protected[this] def readerCaseFunction[T](names: Array[String],
                                            defaults: Array[Js.Value],
                                            read: PartialFunction[Js.Value, T]): PartialFunction[Js.Value, T] = {
    validate("Object"){case x: Js.Obj => read(mapToArray(x, names, defaults))}
  }
  private[this] def arrayToMap(a: Js.Arr, names: Array[String], defaults: Array[Js.Value]) = {

    val accumulated = new Array[(String, Js.Value)](names.length)
    var i = 0
    val l = a.value.length
    while(i < l){
      if (defaults(i) != a.value(i)){
        accumulated(i) = (names(i) -> a.value(i))
      }
      i += 1
    }
    Js.Obj(accumulated.filter(_ != null):_*)

  }
  private[this] def mapToArray(o: Js.Obj, names: Array[String], defaults: Array[Js.Value]) = {
    val accumulated = new Array[Js.Value](names.length)
    val map = o.value.toMap
    var i = 0
    val l = names.length
    while(i < l){
      if (map.contains(names(i))) accumulated(i) = map(names(i))
      else if (defaults(i) != null) accumulated(i) = defaults(i)
      else throw new Invalid.Data(o, "Key Missing: " + names(i))
      i += 1
    }
    Js.Arr(accumulated:_*)
  }
  protected[this] def RCase[T](names: Array[String],
                             defaults: Array[Js.Value],
                             read: PartialFunction[Js.Value, T]) = {
    Reader[T](readerCaseFunction(names, defaults, read))
  }

  protected[this] def WCase[T](names: Array[String],
                               defaults: Array[Js.Value],
                               write: T => Js.Value) = {
    Writer[T](x => arrayToMap(write(x).asInstanceOf[Js.Arr], names, defaults))
  }
}
