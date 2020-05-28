package upickle.implicits

import upickle.core.{ Visitor, ObjVisitor }

import deriving._, compiletime._

trait ReadersVersionSpecific extends CaseClassReaderPiece:
  this: upickle.core.Types with Readers =>

  def macroRW[T]: ReadWriter[T] = ???


  inline given macroReaderProduct[T](using m: Mirror.ProductOf[T]) as Reader[T] =
    val visitors: List[(String, Visitor[_, _])] =
      inferVisitors[m.MirroredElemLabels, m.MirroredElemTypes]
    val defaultParams: Map[String, AnyRef] = getDefaultParams[T]

    mkReader(visitors.toMap, builder => {
      val values: List[AnyRef] = visitors.map { case (fieldName, _) =>
        builder.getOrElse(fieldName, defaultParams(fieldName)).asInstanceOf[AnyRef] }
      m.fromProduct(ArrayProduct(values.toArray))
    })

  inline given macroReaderSum[T](using m: Mirror.SumOf[T]) as Reader[T] =
    val readers: List[Reader[_ <: T]] = inferReaders[m.MirroredElemTypes]
      .asInstanceOf[List[Reader[_ <: T]]]
    Reader.merge[T](readers:_*)


  private inline def inferReaders[Types <: Tuple]: List[Reader[_]] =
    inline erasedValue[Types] match
      case _: (tpe *: ts) => summonInline[Reader[tpe]] :: inferReaders[ts]
      case _ => Nil

  private inline def inferVisitors[Fields <: Tuple, Types <: Tuple]
    : List[(String, Visitor[_, _])] =
    inline erasedValue[Fields] match
      case _: (field *: fs) =>
        inline erasedValue[Types] match
          case _: (tpe *: ts) =>
            val fieldName: String = constValue[field].asInstanceOf[String]
            val visitor: Reader[tpe] = summonInline[Reader[tpe]]
            (fieldName, visitor) :: inferVisitors[fs, ts]
        end match
      case _ => Nil
end ReadersVersionSpecific
