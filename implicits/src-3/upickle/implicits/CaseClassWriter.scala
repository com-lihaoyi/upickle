package upickle.implicits

import compiletime.{summonInline}
import deriving.Mirror
import scala.reflect.ClassTag
import upickle.core.{ Visitor, ObjVisitor, Annotator }

trait CaseClassWriterPiece extends MacrosCommon:
  this: upickle.core.Types with Writers with Annotator =>
  transparent inline def thisOuter = this
  class CaseClassWriter[V](
    elemsInfo: V => List[(String, Writer[_], Any)],
    defaultParams: Map[String, AnyRef]) extends CaseW[V]:
    def length(v: V): Int =
      var n = 0
      for
        (name, _, value) <- elemsInfo(v)
        defaultValue = defaultParams.get(name)
        if serializeDefaults || defaultValue.isEmpty || defaultValue.get != value
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

  class EnumWriter[T] extends Writer[T]:
    override def write0[V](out: Visitor[_, V], v: T): V = out.visitString(v.toString, -1)
  end EnumWriter

  inline def macroW[T: ClassTag](using m: Mirror.Of[T]): Writer[T] = inline m match {
    case m: Mirror.ProductOf[T] =>
      def writer = new CaseW[T] {
        def length(v: T) =
          def elemsInfo(v: T): List[(String, Writer[_], Any)] =
            val labels: List[String] = macros.fieldLabels[T].map(_._1)
            val writers: List[Writer[_]] =
              macros.summonList[Tuple.Map[m.MirroredElemTypes, Writer]]
                .asInstanceOf[List[Writer[_]]]
            val values: List[Any] = v.asInstanceOf[Product].productIterator.toList
            for ((l, w), v) <- labels.zip(writers).zip(values)
              yield (l, w, v)

          val defaultParams =
            macros.getDefaultParams[T]

          var n = 0
          for
            (name, _, value) <- elemsInfo(v)
            defaultValue = defaultParams.get(name)
            if serializeDefaults || defaultValue.isEmpty || defaultValue.get != value
          do n += 1
          n

        def writeToObject[R](ctx: _root_.upickle.core.ObjVisitor[_, R], v: T): Unit =
          macros.writeSnippets[R, T, Tuple.Map[m.MirroredElemTypes, Writer]](
            thisOuter,
            this,
            v,
            ctx,
            macros.summonList[Tuple.Map[m.MirroredElemTypes, Writer]]
          )
      }

      if macros.isSingleton[T] then annotate(SingletonW[T](null.asInstanceOf[T]), macros.fullClassName[T])
      else if macros.isMemberOfSealedHierarchy[T] then annotate(writer, macros.fullClassName[T])
      else writer

    case _: Mirror.SumOf[T] =>
      inline compiletime.erasedValue[T] match {
        case _: scala.reflect.Enum => new EnumWriter[T]
        case _ =>
          val writers: List[Writer[_ <: T]] = macros.summonList[Tuple.Map[m.MirroredElemTypes, Writer]]
          .asInstanceOf[List[Writer[_ <: T]]]
          Writer.merge[T](writers:_*)
      }
  }

  inline given [T <: Singleton: Mirror.Of: ClassTag]: Writer[T] = macroW[T]

  // see comment in MacroImplicits as to why Dotty's extension methods aren't used here
  implicit class WriterExtension(r: Writer.type):
    inline def derived[T](using Mirror.Of[T], ClassTag[T]): Writer[T] = macroW[T]
  end WriterExtension

end CaseClassWriterPiece
