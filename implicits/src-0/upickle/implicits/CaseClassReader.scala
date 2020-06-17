package upickle.implicits

import deriving._, compiletime._

import upickle.core.{ Visitor, ObjVisitor, Annotator }

trait CaseClassReaderPiece:
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
        currentKey = v.asInstanceOf[String]

      def visitValue(v: Any, index: Int): Unit =
        builder(currentKey) = v

      def visitEnd(index: Int): T =
        make(builder.toMap)
    }
  end CaseClassReader

  inline given [T](using m: Mirror.ProductOf[T]) as Reader[T] =
    val labels: List[String] =
      constValueList[m.MirroredElemLabels].asInstanceOf[List[String]]
    val visitors: List[Visitor[_, _]] =
      summonAll[Tuple.Map[m.MirroredElemTypes, Reader]]
        .asInstanceOf[List[upickle.core.Visitor[_, _]]]
    val defaultParams: Map[String, AnyRef] = getDefaultParams[T]

    val reader = new CaseClassReader[T] {
      override def visitorForKey(key: String) =
        labels.zip(visitors).toMap.apply(key)
      override def make(params: Map[String, Any]): T =
        val values: List[AnyRef] = labels.zip(visitors).map { case (fieldName, _) =>
          params.getOrElse(fieldName, defaultParams(fieldName)).asInstanceOf[AnyRef] }
        m.fromProduct(ArrayProduct(values.toArray))
      end make
    }

    if isMemberOfSealedHierarchy[T] then annotate(reader, fullClassName[T])
    else reader
  end given

  inline given [T](using m: Mirror.SumOf[T]) as Reader[T] =
    val readers: List[Reader[_ <: T]] = summonAll[Tuple.Map[m.MirroredElemTypes, Reader]]
      .asInstanceOf[List[Reader[_ <: T]]]
    Reader.merge[T](readers:_*)
  end given
end CaseClassReaderPiece
