package upickle



import upickle.internal.IndexedJs
import upickle.jawn._

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
  override def taggedArrayContext[T](taggedReader: TaggedReader[T], index: Int) = new RawFContext[Any, T] {
    var state: TaggedReaderState = TaggedReaderState.Initializing

    def visitKey(s: CharSequence, index: Int): Unit = throw new Exception(s + " " + index)

    def facade = state match{
      case TaggedReaderState.Initializing => StringReader
      case TaggedReaderState.Parsing(f) => f
      case TaggedReaderState.Parsed(res) => NullFacade
    }

    def add(v: Any, index: Int): Unit = state match{
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

    def finish(index: Int) = state match{
      case TaggedReaderState.Parsed(res) => res.asInstanceOf[T]
      case _ => throw new AbortJsonProcessingException("expected tagged dictionary")
    }

    def isObj = false
  }
  def taggedWrite[T, R](w: CaseW[T], tag: String, out: Facade[R], v: T): R = {
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
    case class FastPath(ctx: RawFContext[Any, _]) extends TaggedReaderState
    case class SlowPath(ctx: RawFContext[Any, IndexedJs.Obj]) extends TaggedReaderState
  }
  override def taggedObjectContext[T](taggedReader: TaggedReader[T], index: Int) = {
    new upickle.jawn.RawFContext[Any, T]{
      var state: TaggedReaderState = TaggedReaderState.Initializing
      def visitKey(s: CharSequence, index: Int): Unit = state match{
        case TaggedReaderState.Initializing =>
          if (s.toString == tagName) () //do nothing
          else {
            val slowCtx = IndexedJsObjR.objectContext(index)
            slowCtx.visitKey(s, index)
            state = TaggedReaderState.SlowPath(slowCtx)
          }
        case TaggedReaderState.FastPath(ctx) => ctx.visitKey(s, index)
        case TaggedReaderState.SlowPath(ctx) => ctx.visitKey(s, index)
      }

      def facade = state match{
        case TaggedReaderState.Initializing => StringReader
        case TaggedReaderState.FastPath(ctx) => ctx.facade
        case TaggedReaderState.SlowPath(ctx) => ctx.facade
      }

      def add(v: Any, index: Int): Unit = state match{
        case TaggedReaderState.Initializing =>
          val typeName = v.toString
          val facade0 = taggedReader.findReader(typeName)
          if (facade0 == null) {
            throw new AbortJsonProcessingException("invalid tag for tagged object: " + typeName)
          }
          state = TaggedReaderState.FastPath(facade0.objectContext(index))
        case TaggedReaderState.FastPath(ctx) => ctx.add(v, index)
        case TaggedReaderState.SlowPath(ctx) => ctx.add(v, index)
      }

      def finish(index: Int) = state match{
        case TaggedReaderState.Initializing => throw new AbortJsonProcessingException("expected tagged dictionary")
        case TaggedReaderState.FastPath(ctx) => ctx.finish(index).asInstanceOf[T]
        case TaggedReaderState.SlowPath(ctx) =>
          val x = ctx.finish(index)
          val keyAttr = x.value0.find(_._1.toString == tagName).get._2
          val key = keyAttr.asInstanceOf[IndexedJs.Str].value0.toString
          val delegate = taggedReader.findReader(key)
          if (delegate == null){
            throw new JsonProcessingException("invalid tag for tagged object: " + key, keyAttr.index, -1, -1, Nil, null)
          }
          val ctx2 = delegate.objectContext(-1)
          for (p <- x.value0) {
            val (k0, v) = p
            val k = k0.toString
            if (k != tagName){
              ctx2.visitKey(k, -1)
              ctx2.add(visitors.JsVisitor.visit(v, ctx2.facade), -1)
            }
          }
          ctx2.finish(index)
      }

      def isObj = true
    }
  }
  def taggedWrite[T, R](w: CaseW[T], tag: String, out: Facade[R], v: T): R = {
    val ctx = out.objectContext(-1)
    ctx.visitKey(tagName, -1)
    ctx.add(out.jstring(tag, -1), -1)
    w.writeToObject(ctx, out, v)
    ctx.finish(-1)
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