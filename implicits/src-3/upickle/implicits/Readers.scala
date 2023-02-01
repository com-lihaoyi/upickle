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
                      defaultParams: Array[() => Any]) extends CaseR[T] {


    lazy val visitors = visitors0
    lazy val indexToKey = keyToIndex.map(_.swap)

    def make(params: Array[Any],
             definedParams: Array[Boolean],
             defaults: Array[() => Any]): T =
      val missingKeys = collection.mutable.ListBuffer.empty[String]

      var i = 0
      while (i < params.length)
        if !definedParams(i) then
          defaults(i) match
            case null => missingKeys += indexToKey(i)
            case computeDefault => params(i) = computeDefault()
        i += 1

      if !missingKeys.isEmpty then
        throw new upickle.core.Abort("missing keys in dictionary: " + missingKeys.mkString(", "))

      fromProduct(new Product {
        def canEqual(that: Any): Boolean = true

        def productArity: Int = params.length

        def productElement(i: Int): Any = params(i)
      })

    override def visitObject(length: Int, jsonableKeys: Boolean, index: Int) = new ObjVisitor[Any, T] {
      private val params = new Array[Any](keyToIndex.size)
      private val definedParams = new Array[Boolean](keyToIndex.size)
      var currentIndex: Int = -1

      def subVisitor: Visitor[_, _] =
        if (currentIndex == -1) upickle.core.NoOpVisitor
        else visitors.productElement(currentIndex).asInstanceOf[Visitor[_, _]]

      def visitKey(index: Int): Visitor[_, _] = StringReader

      def visitKeyValue(v: Any): Unit =
        val k = objectAttributeKeyReadMap(v.asInstanceOf[String]).toString
        currentIndex = keyToIndex.getOrElse(k, -1)

      def visitValue(v: Any, index: Int): Unit =
        if (currentIndex != -1)
          params(currentIndex) = v
          definedParams(currentIndex) = true

      def visitEnd(index: Int): T = make(params, definedParams, defaultParams)
    }

    override def visitString(v: CharSequence, index: Int) = make(Array(), Array(), Array())
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
        macros.getDefaultParamsArray[T]
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
