package upickle.implicits

import compiletime.{summonInline}
import deriving.{ArrayProduct, Mirror}
import upickle.core.{ Visitor, ObjVisitor, Annotator }

trait CaseClassReaderPiece extends MacrosCommon:
  this: upickle.core.Types with Readers with Annotator =>
  trait CaseClassReader[T] extends CaseR[T]:
    def make(bldr: Map[String, Any]): T

    def visitorForKey(currentKey: String): Visitor[_, _]

    override def expectedMsg = "expected case class"

    override def visitObject(length: Int, index: Int) = new ObjVisitor[Any, T] {
      var currentKey: String = null
      private val builder = collection.mutable.Map.empty[String, Any]

      def subVisitor: Visitor[_, _] = visitorForKey(currentKey)

      def visitKey(index: Int): Visitor[_, _] = StringReader

      def visitKeyValue(v: Any): Unit =
        currentKey = objectAttributeKeyReadMap(v.asInstanceOf[String]).toString

      def visitValue(v: Any, index: Int): Unit =
        builder(currentKey) = v

      def visitEnd(index: Int): T =
        make(builder.toMap)
    }
  end CaseClassReader

  inline def macroR[T](using m: Mirror.Of[T]): Reader[T] = inline m match {
    case m: Mirror.ProductOf[T] =>
      val labels: List[String] = fieldLabels[T]
      val visitors: List[Visitor[_, _]] =
        summonList[Tuple.Map[m.MirroredElemTypes, Reader]]
          .asInstanceOf[List[upickle.core.Visitor[_, _]]]
      val defaultParams: Map[String, AnyRef] = getDefaultParams[T]

      val reader = new CaseClassReader[T] {
        override def visitorForKey(key: String) =
          labels.zip(visitors).toMap.apply(key)
        override def make(params: Map[String, Any]): T =
          val values: List[AnyRef] = labels.zip(visitors).map { case (fieldName, _) =>
            params.getOrElse(fieldName, defaultParams(fieldName)).asInstanceOf[AnyRef]
          }
          m.fromProduct(ArrayProduct(values.toArray))
        end make
      }

      if isMemberOfSealedHierarchy[T] then annotate(reader, fullClassName[T])
      else reader

    case m: Mirror.SumOf[T] =>
      val readers: List[Reader[_ <: T]] = summonList[Tuple.Map[m.MirroredElemTypes, Reader]]
        .asInstanceOf[List[Reader[_ <: T]]]
      Reader.merge[T](readers:_*)
  }

end CaseClassReaderPiece
