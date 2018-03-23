package upickle



import upickle.internal.IndexedJs
import upickle.json._
import upickle.json.StringRenderer

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
trait Api extends upickle.core.Types with api.Implicits with WebJson{
  def read[T: Reader](s: Transformable) = s.transform(implicitly[Reader[T]])

  def readJs[T: Reader](s: Js.Value) = s.transform(implicitly[Reader[T]])

  def reader[T: Reader] = implicitly[Reader[T]]

  def write[T: Writer](t: T, indent: Int = -1) = {
    transform(t).to(StringRenderer(indent)).toString
  }

  def writeJs[T: Writer](t: T) = transform(t).to[Js.Value]

  def writeTo[T: Writer](t: T, out: java.io.Writer, indent: Int = -1): Unit = {
    transform(t).to(new Renderer(out, indent = indent))
  }

  def writer[T: Writer] = implicitly[Writer[T]]

  def writable[T: Writer](t: T) = Transformable.fromTransformer(t, implicitly[Writer[T]])

  case class transform[T: Writer](t: T) extends Transformable{
    def transform[V](f: upickle.json.Visitor[_, V]): V = implicitly[Writer[T]].transform(t, f)
    def to[V](f: upickle.json.Visitor[_, V]): V = implicitly[Writer[T]].transform(t, f)
    def to[V](implicit f: Reader[V]): V = implicitly[Writer[T]].transform(t, f)
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
        val typeName = v.toString
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
    ctx.visitValue(out.visitString(tag, -1), -1)

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
  sealed trait TaggedReaderState
  object TaggedReaderState{
    case object Initializing extends TaggedReaderState
    case class FastPath(ctx: ObjVisitor[Any, _]) extends TaggedReaderState
    case class SlowPath(ctx: ObjVisitor[Any, IndexedJs.Obj]) extends TaggedReaderState
  }
  override def taggedObjectContext[T](taggedReader: TaggedReader[T], index: Int) = {
    new upickle.json.ObjVisitor[Any, T]{
      var state: TaggedReaderState = TaggedReaderState.Initializing
      def visitKey(s: CharSequence, index: Int): Unit = state match{
        case TaggedReaderState.Initializing =>
          if (s.toString == tagName) () //do nothing
          else {
            val slowCtx = IndexedJs.Builder.visitObject(index).narrow
            slowCtx.visitKey(s, index)
            state = TaggedReaderState.SlowPath(slowCtx)
          }
        case TaggedReaderState.FastPath(ctx) => ctx.visitKey(s, index)
        case TaggedReaderState.SlowPath(ctx) => ctx.visitKey(s, index)
      }

      def subVisitor = state match{
        case TaggedReaderState.Initializing => StringReader
        case TaggedReaderState.FastPath(ctx) => ctx.subVisitor
        case TaggedReaderState.SlowPath(ctx) => ctx.subVisitor
      }

      def visitValue(v: Any, index: Int): Unit = state match{
        case TaggedReaderState.Initializing =>
          val typeName = v.toString
          val facade0 = taggedReader.findReader(typeName)
          if (facade0 == null) {
            throw new AbortJsonProcessingException("invalid tag for tagged object: " + typeName)
          }
          state = TaggedReaderState.FastPath(facade0.visitObject(index))
        case TaggedReaderState.FastPath(ctx) => ctx.visitValue(v, index)
        case TaggedReaderState.SlowPath(ctx) => ctx.visitValue(v, index)
      }

      def visitEnd(index: Int) = state match{
        case TaggedReaderState.Initializing => throw new AbortJsonProcessingException("expected tagged dictionary")
        case TaggedReaderState.FastPath(ctx) => ctx.visitEnd(index).asInstanceOf[T]
        case TaggedReaderState.SlowPath(ctx) =>
          val x = ctx.visitEnd(index)
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
  def taggedWrite[T, R](w: CaseW[T], tag: String, out: Visitor[_,  R], v: T): R = {
    val ctx = out.asInstanceOf[Visitor[Any, R]].visitObject(-1)
    ctx.visitKey(tagName, -1)
    ctx.visitValue(out.visitString(tag, -1), -1)
    w.writeToObject(ctx, out, v)
    ctx.visitEnd(-1)
  }
}


case class key(s: String) extends StaticAnnotation