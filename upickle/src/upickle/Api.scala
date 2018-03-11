package upickle



import language.experimental.macros
import language.higherKinds
/**
 * An instance of the upickle API. There's a default instance at
 * `upickle.default`, but you can also implement it yourself to customize
 * its behavior. Override the `annotate` methods to control how a sealed
 * trait instance is tagged during reading and writing.
 */
trait Api extends Types with Implicits {
  def read[T: Reader](s: String) = {
    jawn.Parser.parseUnsafe(s)(implicitly[Reader[T]])
  }
  def write[T: Writer](t: T) = {
    val out = new java.io.StringWriter()
    implicitly[Writer[T]].write(new Renderer(out), t)
    out.toString
  }
//  def annotate[V: ClassTag](rw: Reader[V], n: String): Reader[V]
//  def annotate[V: ClassTag](rw: Writer[V], n: String): Writer[V]
}

/**
 * The default way of accessing upickle
 */
object default extends AttributeTagged{

}
/**
 * An instance of the upickle API that follows the old serialization for
 * tagged instances of sealed traits.
 */
object legacy extends Api{
//  def annotate[V: ClassTag](rw: Reader[V], n: String) = Reader[V]{
//    case Js.Arr(Js.Str(`n`), x) => rw.read(x)
//  }
//
//  def annotate[V: ClassTag](rw: Writer[V], n: String) = Writer[V]{
//    case x: V => Js.Arr(Js.Str(n), rw.write(x))
//  }
}

/**
 * A `upickle.Api` that follows the default sealed-trait-instance-tagging
 * behavior of using an attribute, but allow you to control what the name
 * of the attribute is.
 */
trait AttributeTagged extends Api{
  def tagName = "$type"
//  def annotate[V: ClassTag](rw: Reader[V], n: String) = Reader[V]{
//    case Js.Obj(x@_*) if x.contains((tagName, Js.Str(n))) =>
//    rw.read(Js.Obj(x.filter(_._1 != tagName):_*))
//
//  }
//
//  def annotate[V: ClassTag](rw: Writer[V], n: String) = Writer[V]{ case x: V =>
//    Js.Obj((tagName, Js.Str(n)) +: rw.write(x).asInstanceOf[Js.Obj].value:_*)
//  }
}

object json{
  val jsRW = upickle.default.macroRW0[Js.Value](implicitly, implicitly)
  def read(s: String) = jawn.Parser.parseUnsafe(s)(jsRW)
  def read(s: java.nio.ByteBuffer) = jawn.Parser.parseFromByteBuffer(s)(jsRW).get
  def read(s: java.io.File) = jawn.Parser.parseFromFile(s)(jsRW).get
  def write(t: Js.Value): String = {
    val out = new java.io.StringWriter()
    jsRW.write(new Renderer(out), t)
    out.toString
  }
}