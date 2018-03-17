package upickle



import upickle.jawn.{Facade, RawFContext, RawFacade}

import language.experimental.macros
import language.higherKinds
import scala.annotation.StaticAnnotation
import scala.reflect.ClassTag
/**
 * An instance of the upickle API. There's a default instance at
 * `upickle.default`, but you can also implement it yourself to customize
 * its behavior. Override the `annotate` methods to control how a sealed
 * trait instance is tagged during reading and writing.
 */
trait Api extends upickle.core.Types with api.Implicits {
  def read[T: Reader](s: String) = {
    upickle.jawn.Parser.parseUnsafe(s)(implicitly[Reader[T]])
  }
  def write[T: Writer](t: T, indent: Int = -1) = {
    val out = new java.io.StringWriter()
    implicitly[Writer[T]].write(new visitors.Renderer(out, indent = indent), t)
    out.toString
  }
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
  def annotate[V](rw: Reader[V], n: String) = new TaggedReader.Leaf[V](n, rw)

  def annotate[V](rw: Writer[V], n: String)(implicit c: ClassTag[V]) = {
    new TaggedWriter.Leaf[V](c, n, rw)
  }

  override def taggedArrayContext[T](taggedReader: TaggedReader[T], index: Int) = new RawFContext[Any, T] {
    var typeName: String = null
    var delegate: Reader[_] = null
    var delegateCtx: RawFContext[_, T] = null

    var res: T = _
    def visitKey(s: CharSequence, index: Int): Unit = throw new Exception(s + " " + index)

    def facade =
      if (typeName == null) StringReader
      else delegate

    def add(v: Any, index: Int): Unit = {
      if (typeName == null){
        typeName = v.toString
        delegate = taggedReader.findReader(typeName)
      }else{
        res = v.asInstanceOf[T]
      }
    }

    def finish(index: Int) = {
      res
    }

    def isObj = false
  }
  def taggedWrite[T, R](w: Writer[T], tag: String, out: Facade[R], v: T): R = {
    val ctx = out.arrayContext(-1)
    ctx.add(out.jstring(tag, -1), -1)

    ctx.add(w.write(out, v), -1)

    ctx.finish(-1)
  }
}

/**
 * A `upickle.Api` that follows the default sealed-trait-instance-tagging
 * behavior of using an attribute, but allow you to control what the name
 * of the attribute is.
 */
trait AttributeTagged extends Api{
  def tagName = "$type"
  def annotate[V](rw: Reader[V], n: String) = {
    new TaggedReader.Leaf[V](n, rw)
  }

  def annotate[V](rw: Writer[V], n: String)(implicit c: ClassTag[V]) = {
    new TaggedWriter.Leaf[V](c, n, rw)
  }

  override def taggedObjectContext[T](taggedReader: TaggedReader[T], index: Int) = {
    upickle.core.Util.mapContext(JsObjR.objectContext(index)) { x =>
      val key = x.value.find(_._1 == tagName).get._2.str.toString
      val delegate = taggedReader.findReader(key)

      val ctx = delegate.objectContext(-1)
      for ((k, v) <- x.value if k != tagName) {
        ctx.visitKey(k, -1)
        ctx.add(visitors.JsVisitor.visit(v, ctx.facade), -1)
      }
      ctx.finish(index)
    }
  }
  def taggedWrite[T, R](w: Writer[T], tag: String, out: Facade[R], v: T): R = {
    val tree = w.write(visitors.JsBuilder, v)
    val Js.Obj(kvs @ _*) = tree
    val tagged = Js.Obj((tagName -> Js.Str(tag)) +: kvs: _*)
    visitors.JsVisitor.visit(tagged, out)
  }
}

object json{
  val jsRW = upickle.default.macroRW0[Js.Value](implicitly, implicitly)
  def read(s: String) = upickle.jawn.Parser.parseUnsafe(s)(jsRW)
  def read(s: java.nio.ByteBuffer) = upickle.jawn.Parser.parseFromByteBuffer(s)(jsRW).get
  def read(s: java.io.File) = upickle.jawn.Parser.parseFromFile(s)(jsRW).get
  def write(t: Js.Value): String = {
    val out = new java.io.StringWriter()
    jsRW.write(new visitors.Renderer(out), t)
    out.toString
  }
}

case class key(s: String) extends StaticAnnotation