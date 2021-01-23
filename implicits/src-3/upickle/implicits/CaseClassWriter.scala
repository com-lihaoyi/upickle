package upickle.implicits

import compiletime.{summonInline}
import deriving.Mirror
import scala.reflect.ClassTag
import upickle.core.{ Visitor, ObjVisitor, Annotator }

trait CaseClassWriterPiece extends MacrosCommon:
  this: upickle.core.Types with Writers with Annotator =>
  class CaseClassWriter[V](
    elemsInfo: V => List[(String, Writer[_], Any)],
    defaultParams: Map[String, AnyRef]) extends CaseW[V]:
    def length(v: V): Int =
      var n = 0
      for
        (name, _, value) <- elemsInfo(v)
        defaultValue <- defaultParams.get(name)
        if defaultValue != value || serializeDefaults
      do n += 1
      n
    end length

    def writeToObject[R](ctx: ObjVisitor[_, R], v: V): Unit =
      for
        (name, writer, value) <- elemsInfo(v)
        defaultValue = defaultParams.get(name)
        if serializeDefaults || defaultValue.isEmpty || defaultValue.get != value
      do
        val keyVisitor = ctx.visitKey(-1)
        ctx.visitKeyValue(
          keyVisitor.visitString(
            objectAttributeKeyWriteMap(name),
            -1
          )
        )
        ctx.narrow.visitValue(
          writer.narrow.write(ctx.subVisitor, value), -1)
    end writeToObject
  end CaseClassWriter

  inline def macroW[T: ClassTag](using m: Mirror.Of[T]): Writer[T] = inline m match {
    case m: Mirror.ProductOf[T] =>
      def elemsInfo(v: T): List[(String, Writer[_], Any)] =
        val labels: List[String] = macros.fieldLabels[T]
        val writers: List[Writer[_]] =
          macros.summonList[Tuple.Map[m.MirroredElemTypes, Writer]]
            .asInstanceOf[List[Writer[_]]]
        val values: List[Any] = v.asInstanceOf[Product].productIterator.toList
        for ((l, w), v) <- labels.zip(writers).zip(values)
        yield (l, w, v)
      end elemsInfo
      val writer = CaseClassWriter[T](elemsInfo, macros.getDefaultParams[T])

      if macros.isMemberOfSealedHierarchy[T] then annotate(writer, macros.fullClassName[T])
      else writer
    case m: Mirror.SumOf[T] =>
      val writers: List[Writer[_ <: T]] = macros.summonList[Tuple.Map[m.MirroredElemTypes, Writer]]
        .asInstanceOf[List[Writer[_ <: T]]]
      Writer.merge[T](writers:_*)
  }

  inline given [T <: Singleton: Mirror.Of: ClassTag]: Writer[T] = macroW[T]

  // see comment in MacroImplicits as to why Dotty's extension methods aren't used here
  implicit class WriterExtension(r: Writer.type):
    inline def derived[T](using Mirror.Of[T], ClassTag[T]): Writer[T] = macroW[T]
  end WriterExtension

end CaseClassWriterPiece
