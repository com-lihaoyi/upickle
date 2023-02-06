package upickle.implicits

import compiletime.summonInline
import deriving.Mirror
import upickle.core.{Annotator, ObjVisitor, Visitor, Abort}
import upickle.implicits.macros.EnumDescription

trait ReadersVersionSpecific extends MacrosCommon:
  this: upickle.core.Types with Readers with Annotator =>

  class CaseReader[T](visitors0: => Product,
                      fromProduct: Product => T,
                      keyToIndex: Map[String, Int],
                      defaultParams: Array[() => Any],
                      missingKeyCount: Long) extends CaseR[T] {

    val paramCount = keyToIndex.size
    lazy val visitors = visitors0
    lazy val indexToKey = keyToIndex.map(_.swap)

    trait ObjectContext extends ObjVisitor[Any, T] with BaseCaseObjectContext{
      private val params = new Array[Any](paramCount)

      def storeAggregatedValue(currentIndex: Int, v: Any): Unit = params(currentIndex) = v

      def subVisitor: Visitor[_, _] =
        if (currentIndex == -1) upickle.core.NoOpVisitor
        else visitors.productElement(currentIndex).asInstanceOf[Visitor[_, _]]

      def visitKeyValue(v: Any): Unit =
        val k = objectAttributeKeyReadMap(v.toString).toString
        currentIndex = keyToIndex.getOrElse(k, -1)

      def visitEnd(index: Int): T =
        var i = 0
        while (i < paramCount)
          defaultParams(i) match
            case null =>
            case computeDefault => storeValueIfNotFound(i, computeDefault())

          i += 1

        // Special-case 64 because java bit shifting ignores any RHS values above 63
        // https://docs.oracle.com/javase/specs/jls/se7/html/jls-15.html#jls-15.19
        if (this.checkErrorMissingKeys(missingKeyCount))
          this.errorMissingKeys(paramCount, indexToKey.toSeq.sortBy(_._1).map(_._2).toArray)

        fromProduct(new Product {
          def canEqual(that: Any): Boolean = true
          def productArity: Int = params.length
          def productElement(i: Int): Any = params(i)
        })
    }
    override def visitObject(length: Int,
                             jsonableKeys: Boolean,
                             index: Int) =
      if (paramCount <= 64) new CaseObjectContext(paramCount) with ObjectContext
      else new HugeCaseObjectContext(paramCount) with ObjectContext
  }

  class EnumReader[T](f: String => T, description: EnumDescription) extends SimpleReader[T] :
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

      val reader = new CaseReader(
        compiletime.summonAll[Tuple.Map[m.MirroredElemTypes, Reader]],
        m.fromProduct(_),
        macros.fieldLabels[T].map(_._2).zipWithIndex.toMap,
        macros.getDefaultParamsArray[T],
        macros.checkErrorMissingKeysCount[T]()
      )

      if macros.isMemberOfSealedHierarchy[T] then annotate(reader, macros.fullClassName[T])
      else reader

    case m: Mirror.SumOf[T] =>
      inline compiletime.erasedValue[T] match {
        case _: scala.reflect.Enum =>
          val valueOf = macros.enumValueOf[T]
          val description = macros.enumDescription[T]
          new EnumReader[T](valueOf, description)
        case _ =>
          val readers: List[Reader[_ <: T]] = compiletime.summonAll[Tuple.Map[m.MirroredElemTypes, Reader]]
            .toList
            .asInstanceOf[List[Reader[_ <: T]]]

          Reader.merge[T](readers: _*)
      }
  }

  inline given[T <: Singleton : Mirror.Of]: Reader[T] = macroR[T]

  // see comment in MacroImplicits as to why Dotty's extension methods aren't used here
  implicit class ReaderExtension(r: Reader.type):
    inline def derived[T](using Mirror.Of[T]): Reader[T] = macroR[T]
  end ReaderExtension
end ReadersVersionSpecific
