package upickle.implicits

import compiletime.summonInline
import deriving.Mirror
import upickle.core.{Annotator, ObjVisitor, Visitor, Abort}
import upickle.implicits.macros.EnumDescription

trait CaseClassReaderPiece extends MacrosCommon:
  this: upickle.core.Types with Readers with Annotator =>
  trait CaseClassReader[T] extends CaseR[T]:
    def make(bldr: Map[String, Any]): T

    def visitorForKey(currentKey: String): Visitor[_, _]

    override def visitObject(length: Int, index: Int) = new ObjVisitor[Any, T] {
      private val builder = collection.mutable.Map.empty[String, Any]
      var currentKey: String = null

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

  class EnumReader[T](f: String => T, description: EnumDescription) extends SimpleReader[T]:
    override def expectedMsg = "expected string enumeration"
    override def visitString(s: CharSequence, index: Int) = {
      val str = s.toString
      try {
        f(str)
      } catch {
        case _: IllegalArgumentException =>
          throw new Abort(s"Value '$str' was not found in enumeration ${description.pretty}")
      }
    }
  end EnumReader

  inline def macroR[T](using m: Mirror.Of[T]): Reader[T] = inline m match {
    case m: Mirror.ProductOf[T] =>
      val labels: List[String] = macros.fieldLabels[T]
      val visitors: List[Visitor[_, _]] =
        macros.summonList[Tuple.Map[m.MirroredElemTypes, Reader]]
          .asInstanceOf[List[upickle.core.Visitor[_, _]]]
      val defaultParams: Map[String, AnyRef] = macros.getDefaultParams[T]

      val reader = new CaseClassReader[T] {
        override def visitorForKey(key: String) =
          labels.zip(visitors).toMap.get(key) match {
            case None => upickle.core.NoOpVisitor
            case Some(v) => v
          }

        override def make(params: Map[String, Any]): T =
          val values = collection.mutable.ListBuffer.empty[AnyRef]
          val missingKeys = collection.mutable.ListBuffer.empty[String]

          labels.zip(visitors).map { case (fieldName, _) =>
            params.get(fieldName) match {
              case Some(value) => values += value.asInstanceOf[AnyRef]
              case None =>
                defaultParams.get(fieldName) match {
                  case Some(fallback) => values += fallback.asInstanceOf[AnyRef]
                  case None => missingKeys += fieldName
                }
            }
          }

          if (!missingKeys.isEmpty) {
            throw new upickle.core.Abort("missing keys in dictionary: " + missingKeys.mkString(", "))
          }

          val valuesArray = values.toArray
          m.fromProduct(new Product {
            def canEqual(that: Any): Boolean = true
            def productArity: Int = valuesArray.length
            def productElement(i: Int): Any = valuesArray(i)
          })
        end make
      }

      if macros.isMemberOfSealedHierarchy[T] then annotate(reader, macros.fullClassName[T])
      else reader

    case m: Mirror.SumOf[T] =>
      inline compiletime.erasedValue[T] match {
        case _: scala.reflect.Enum =>
          val valueOf = macros.enumValueOf[T]
          val description = macros.enumDescription[T]
          new EnumReader[T](valueOf, description)
        case _ =>
          val readers: List[Reader[_ <: T]] = macros.summonList[Tuple.Map[m.MirroredElemTypes, Reader]]
            .asInstanceOf[List[Reader[_ <: T]]]
            Reader.merge[T](readers:_*)
      }
  }

  inline given [T <: Singleton: Mirror.Of]: Reader[T] = macroR[T]

  // see comment in MacroImplicits as to why Dotty's extension methods aren't used here
  implicit class ReaderExtension(r: Reader.type):
    inline def derived[T](using Mirror.Of[T]): Reader[T] = macroR[T]
  end ReaderExtension

end CaseClassReaderPiece
