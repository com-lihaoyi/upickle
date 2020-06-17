package upickle.implicits

import scala.reflect.ClassTag
import deriving._, compiletime._

import upickle.core.{ Visitor, ObjVisitor, Annotator }

trait CaseClassWriterPiece:
  this: upickle.core.Types with Writers with Annotator =>
  class CaseClassWriter[V](
    elemsInfo: V => List[(String, Writer[_], Any)],
    defaultParams: Map[String, AnyRef]) extends CaseW[V]:
    def length(v: V): Int =
      var n = 0
      for
        (name, _, value) <- elemsInfo(v)
        defaultValue <- defaultParams.get(name)
        if defaultValue != value
      do n += 1
      n
    end length

    def writeToObject[R](ctx: ObjVisitor[_, R], v: V): Unit =
      for (name, writer, value) <- elemsInfo(v) do
        val keyVisitor = ctx.visitKey(-1)
        ctx.visitKeyValue(
          keyVisitor.visitString(name, -1)
        )
        ctx.narrow.visitValue(
          writer.narrow.write(ctx.subVisitor, value), -1)
    end writeToObject
  end CaseClassWriter

  inline given [T <: Product: ClassTag](using m: Mirror.ProductOf[T]) as Writer[T] =
    def elemsInfo(v: T): List[(String, Writer[_], Any)] =
      val labels: List[String] =
        constValueList[m.MirroredElemLabels]
          .asInstanceOf[List[String]]
      val writers: List[Writer[_]] =
        summonAll[Tuple.Map[m.MirroredElemTypes, Writer]]
          .asInstanceOf[List[Writer[_]]]
      val values: List[Any] = v.productIterator.toList
      for ((l, w), v) <- labels.zip(writers).zip(values)
      yield (l, w, v)
    end elemsInfo
    val writer = CaseClassWriter[T](elemsInfo, getDefaultParams[T])

    if isMemberOfSealedHierarchy[T] then annotate(writer, fullClassName[T])
    else writer
  end given

  inline given [T](using m: Mirror.SumOf[T]) as Writer[T] =
    val writers: List[Writer[_ <: T]] = summonAll[Tuple.Map[m.MirroredElemTypes, Writer]]
      .asInstanceOf[List[Writer[_ <: T]]]
    Writer.merge[T](writers:_*)
  end given
end CaseClassWriterPiece
