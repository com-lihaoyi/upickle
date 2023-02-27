package upickle.implicits

import upickle.core.Annotator

import compiletime.{summonInline}
import deriving.Mirror
import scala.reflect.ClassTag
import upickle.core.{ Visitor, ObjVisitor, Annotator, AnnotatorChecker }

trait WritersVersionSpecific extends MacrosCommon:
  outerThis: upickle.core.Types with Writers with Annotator =>

  inline def macroW[T: ClassTag](using m: Mirror.Of[T]): Writer[T] = inline m match {
    case m: Mirror.ProductOf[T] =>
      def writer = new CaseW[T] {
        def length(v: T) = macros.writeLength[T](outerThis, v)

        def writeToObject[R](ctx: _root_.upickle.core.ObjVisitor[_, R], v: T): Unit =
          macros.writeSnippets[R, T, Tuple.Map[m.MirroredElemTypes, Writer]](
            outerThis,
            this,
            v,
            ctx
          )
      }

      inline if macros.isSingleton[T] then
        annotate[T](SingletonW[T](null.asInstanceOf[T]), macros.tagName[T], AnnotatorChecker.Val(valueOf[T]))
      else if macros.isMemberOfSealedHierarchy[T] then
        annotate[T](writer, macros.tagName[T], AnnotatorChecker.Cls(implicitly[ClassTag[T]].runtimeClass))
      else writer

    case _: Mirror.SumOf[T] =>
      val writers: List[Writer[_ <: T]] = compiletime.summonAll[Tuple.Map[m.MirroredElemTypes, Writer]]
        .toList
        .asInstanceOf[List[Writer[_ <: T]]]

      Writer.merge[T](writers: _*): Writer[T]
  }

  inline given[T <: Singleton : Mirror.Of : ClassTag]: Writer[T] = macroW[T]

  // see comment in MacroImplicits as to why Dotty's extension methods aren't used here
  implicit class WriterExtension(r: Writer.type):
    inline def derived[T](using Mirror.Of[T], ClassTag[T]): Writer[T] = macroW[T]
  end WriterExtension

end WritersVersionSpecific
