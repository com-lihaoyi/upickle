package upickle.implicits

import upickle.core.Annotator

import deriving.Mirror
import scala.reflect.ClassTag
import scala.util.NotGiven
import upickle.core.{Annotator, ObjVisitor, Visitor, CurrentlyDeriving}

trait WritersVersionSpecific
  extends MacrosCommon
    with upickle.core.Types
    with Annotator
    with CaseClassReadWriters:

  val outerThis = this
  inline def macroW[T: ClassTag](using m: Mirror.Of[T]): Writer[T] = inline m match {
    case m: Mirror.ProductOf[T] =>

      def writer = new CaseClassWriter[T] {
        def length(v: T) = macros.writeLength[T](outerThis, v)

        override def write0[R](out: Visitor[_, R], v: T): R = {
          if (v == null) out.visitNull(-1)
          else {
            val ctx = out.visitObject(length(v), true, -1)
            macros.writeSnippets[R, T, Tuple.Map[m.MirroredElemTypes, Writer]](
              outerThis,
              this,
              v,
              ctx
            )
            ctx.visitEnd(-1)
          }
        }

        def writeToObject[R](ctx: _root_.upickle.core.ObjVisitor[_, R], v: T): Unit =
          macros.writeSnippets[R, T, Tuple.Map[m.MirroredElemTypes, Writer]](
            outerThis,
            this,
            v,
            ctx
          )
      }

      inline if macros.isSingleton[T] then
        annotate[T](SingletonWriter[T](null.asInstanceOf[T]), macros.tagName[T], Annotator.Checker.Val(macros.getSingleton[T]))
      else if macros.isMemberOfSealedHierarchy[T] then
        annotate[T](writer, macros.tagName[T], Annotator.Checker.Cls(implicitly[ClassTag[T]].runtimeClass))
      else writer

    case _: Mirror.SumOf[T] =>
      implicit val currentlyDeriving: upickle.core.CurrentlyDeriving[T] = new upickle.core.CurrentlyDeriving()
      val writers: List[Writer[_ <: T]] = compiletime.summonAll[Tuple.Map[m.MirroredElemTypes, Writer]]
        .toList
        .asInstanceOf[List[Writer[_ <: T]]]

      Writer.merge[T](writers: _*): Writer[T]
  }

  inline def macroWAll[T: ClassTag](using m: Mirror.Of[T]): Writer[T] = inline m match{
    case m: Mirror.ProductOf[T] => macroW[T]
    case m: Mirror.SumOf[T] =>
      macros.defineEnumWriters[Writer[T], Tuple.Map[m.MirroredElemTypes, Writer]](this)
  }

  inline given superTypeWriter[T: Mirror.ProductOf : ClassTag, V >: T : Writer : Mirror.SumOf]
                              (using NotGiven[CurrentlyDeriving[V]]): Writer[T] = {
    implicitly[Writer[V]].comap[T](_.asInstanceOf[V])
  }

  // see comment in MacroImplicits as to why Dotty's extension methods aren't used here
  implicit class WriterExtension(r: Writer.type):
    inline def derived[T](using Mirror.Of[T], ClassTag[T]): Writer[T] = macroWAll[T]
  end WriterExtension


