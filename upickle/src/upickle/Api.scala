package upickle



import upickle.internal.IndexedJs
import ujson._
import ujson.StringRenderer

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
trait Api extends upickle.core.Types with api.Implicits with WebJson with Api.NoOpMappers{
  def read[T: Reader](s: Transformable): T = s.transform(reader[T])

  def readJs[T: Reader](s: Js.Value): T = s.transform(reader[T])

  def reader[T: Reader] = implicitly[Reader[T]]

  def write[T: Writer](t: T, indent: Int = -1, escapeUnicode: Boolean = false): String = {
    transform(t).to(StringRenderer(indent, escapeUnicode)).toString
  }

  def writeJs[T: Writer](t: T): Js.Value = transform(t).to[Js.Value]

  def writeTo[T: Writer](t: T, out: java.io.Writer, indent: Int = -1, escapeUnicode: Boolean = false): Unit = {
    transform(t).to(new Renderer(out, indent = indent, escapeUnicode))
  }

  def writer[T: Writer] = implicitly[Writer[T]]

  def writable[T: Writer](t: T): Transformable = Transformable.fromTransformer(t, writer[T])

  def readwriter[T: ReadWriter] = implicitly[ReadWriter[T]]

  case class transform[T: Writer](t: T) extends Transformable{
    def transform[V](f: ujson.Visitor[_, V]): V = writer[T].transform(t, f)
    def to[V](f: ujson.Visitor[_, V]): V = transform(f)
    def to[V](implicit f: Reader[V]): V = transform(f)
  }
  // End Api
}
object Api{
  trait NoOpMappers{

    def objectAttributeKeyReadMap(s: CharSequence): CharSequence = s
    def objectAttributeKeyWriteMap(s: CharSequence): CharSequence = s

    def objectTypeKeyReadMap(s: CharSequence): CharSequence = s
    def objectTypeKeyWriteMap(s: CharSequence): CharSequence = s
  }
  object StringVisitor extends CustomVisitor[Nothing, Any] {
    def expectedMsg = "expected string"
    override def visitString(s: CharSequence, index: Int) = s
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
object legacy extends LegacyApi
trait LegacyApi extends Api{
  def annotate[V](rw: Reader[V], n: String) = new TaggedReader.Leaf[V](n, rw)

  def annotate[V](rw: CaseW[V], n: String)(implicit c: ClassTag[V]) = {
    new TaggedWriter.Leaf[V](c, n, rw)
  }

  def taggedExpectedMsg = "expected sequence"
  sealed trait TaggedReaderState
  object TaggedReaderState{
    case object Initializing extends TaggedReaderState
    case class Parsing(f: Reader[_]) extends TaggedReaderState
    case class Parsed(res: Any) extends TaggedReaderState
  }
  override def taggedArrayContext[T](taggedReader: TaggedReader[T], index: Int) = new ArrVisitor[Any, T] {
    var state: TaggedReaderState = TaggedReaderState.Initializing

    def subVisitor = state match{
      case TaggedReaderState.Initializing => StringReader
      case TaggedReaderState.Parsing(f) => f
      case TaggedReaderState.Parsed(res) => NoOpVisitor
    }

    def visitValue(v: Any, index: Int): Unit = state match{
      case TaggedReaderState.Initializing =>
        val typeName = objectTypeKeyReadMap(v.toString).toString
        val delegate = taggedReader.findReader(typeName)
        if (delegate == null) {
          throw new AbortJsonProcessingException("invalid tag for tagged object: " + typeName)
        }
        state = TaggedReaderState.Parsing(delegate)
      case TaggedReaderState.Parsing(f) =>
        state = TaggedReaderState.Parsed(v)
      case TaggedReaderState.Parsed(res) => res.asInstanceOf[T]
        throw new AbortJsonProcessingException("expected tagged dictionary")
    }

    def visitEnd(index: Int) = state match{
      case TaggedReaderState.Parsed(res) => res.asInstanceOf[T]
      case _ => throw new AbortJsonProcessingException("expected tagged dictionary")
    }

  }
  def taggedWrite[T, R](w: CaseW[T], tag: String, out: Visitor[_,  R], v: T): R = {
    val ctx = out.asInstanceOf[Visitor[Any, R]].visitArray(-1)
    ctx.visitValue(out.visitString(objectTypeKeyWriteMap(tag), -1), -1)

    ctx.visitValue(w.write(out, v), -1)

    ctx.visitEnd(-1)
  }
}

/**
 * A `upickle.Api` that follows the default sealed-trait-instance-tagging
 * behavior of using an attribute, but allow you to control what the name
 * of the attribute is.
 */
trait AttributeTagged extends Api{
  def tagName = "$type"
  def annotate[V](rw: CaseR[V], n: String) = {
    new TaggedReader.Leaf[V](n, rw)
  }

  def annotate[V](rw: CaseW[V], n: String)(implicit c: ClassTag[V]) = {
    new TaggedWriter.Leaf[V](c, n, rw)
  }

  def taggedExpectedMsg = "expected dictionary"
  override def taggedObjectContext[T](taggedReader: TaggedReader[T], index: Int) = {
    new ujson.ObjVisitor[Any, T]{
      private[this] var fastPath = false
      private[this] var context: ObjVisitor[Any, _] = null
      def subVisitor: Visitor[Nothing, Any] =
        if (context == null) Api.StringVisitor
        else context.subVisitor

      def visitKey(s: CharSequence, index: Int): Unit = {
        if (context != null) context.visitKey(s, index)
        else {
          if (s.toString == tagName) () //do nothing
          else {
            val slowCtx = IndexedJs.Builder.visitObject(index).narrow
            slowCtx.visitKey(s, index)
            context = slowCtx
          }
        }
      }

      def visitValue(v: Any, index: Int): Unit =
        if (context != null) context.visitValue(v, index)
        else {
          val typeName = objectTypeKeyReadMap(v.toString).toString
          val facade0 = taggedReader.findReader(typeName)
          if (facade0 == null) {
            throw new AbortJsonProcessingException("invalid tag for tagged object: " + typeName)
          }
          val fastCtx = facade0.visitObject(index)
          context = fastCtx
          fastPath = true
      }

      def visitEnd(index: Int) = {
        if (context == null) throw new AbortJsonProcessingException("expected tagged dictionary")
        else if (fastPath) context.visitEnd(index).asInstanceOf[T]
        else{
          val x = context.visitEnd(index).asInstanceOf[IndexedJs.Obj]
          val keyAttr = x.value0.find(_._1.toString == tagName).get._2
          val key = keyAttr.asInstanceOf[IndexedJs.Str].value0.toString
          val delegate = taggedReader.findReader(key)
          if (delegate == null){
            throw new JsonProcessingException("invalid tag for tagged object: " + key, keyAttr.index, -1, -1, Nil, null)
          }
          val ctx2 = delegate.visitObject(-1)
          for (p <- x.value0) {
            val (k0, v) = p
            val k = k0.toString
            if (k != tagName){
              ctx2.visitKey(k, -1)
              ctx2.visitValue(IndexedJs.transform(v, ctx2.subVisitor), -1)
            }
          }
          ctx2.visitEnd(index)
        }
      }

    }
  }
  def taggedWrite[T, R](w: CaseW[T], tag: String, out: Visitor[_,  R], v: T): R = {
    val ctx = out.asInstanceOf[Visitor[Any, R]].visitObject(-1)
    ctx.visitKey(tagName, -1)
    ctx.visitValue(out.visitString(objectTypeKeyWriteMap(tag), -1), -1)
    w.writeToObject(ctx, v)
    ctx.visitEnd(-1)
  }
}


case class key(s: String) extends StaticAnnotation