package upickle.implicits

import compiletime.summonInline
import deriving.Mirror
import scala.util.NotGiven
import upickle.core.{Annotator, ObjVisitor, Visitor, Abort, CurrentlyDeriving}
import upickle.implicits.BaseCaseObjectContext

trait ReadersVersionSpecific
  extends MacrosCommon
    with upickle.core.Types
    with Annotator
    with CaseClassReadWriters:

  abstract class CaseClassReadereader[T](paramCount: Int, missingKeyCount: Long) extends CaseClassReader[T] {
    def visitors0: Product
    lazy val visitors = visitors0
    def fromProduct(p: Product): T
    def keyToIndex(x: String): Int
    def allKeysArray: Array[String]
    def storeDefaults(x: upickle.implicits.BaseCaseObjectContext): Unit
    trait ObjectContext extends ObjVisitor[Any, T] with BaseCaseObjectContext{
      private val params = new Array[Any](paramCount)

      def storeAggregatedValue(currentIndex: Int, v: Any): Unit = params(currentIndex) = v

      def subVisitor: Visitor[_, _] =
        if (currentIndex == -1) upickle.core.NoOpVisitor
        else visitors.productElement(currentIndex).asInstanceOf[Visitor[_, _]]

      def visitKeyValue(v: Any): Unit =
        val k = objectAttributeKeyReadMap(v.toString).toString
        currentIndex = keyToIndex(k)

      def visitEnd(index: Int): T =
        storeDefaults(this)

        // Special-case 64 because java bit shifting ignores any RHS values above 63
        // https://docs.oracle.com/javase/specs/jls/se7/html/jls-15.html#jls-15.19
        if (this.checkErrorMissingKeys(missingKeyCount))
          this.errorMissingKeys(paramCount, allKeysArray)

        fromProduct(new Product {
          def canEqual(that: Any): Boolean = true
          def productArity: Int = params.length
          def productElement(i: Int): Any = params(i)
        })
    }
    override def visitObject(length: Int,
                             jsonableKeys: Boolean,
                             index: Int) =
      if (paramCount <= 64) new CaseObjectContext[T](paramCount) with ObjectContext
      else new HugeCaseObjectContext[T](paramCount) with ObjectContext
  }

  inline def macroR[T](using m: Mirror.Of[T]): Reader[T] = inline m match {
    case m: Mirror.ProductOf[T] =>
      val reader = new CaseClassReadereader[T](macros.paramsCount[T], macros.checkErrorMissingKeysCount[T]()){
        override def visitors0 = compiletime.summonAll[Tuple.Map[m.MirroredElemTypes, Reader]]
        override def fromProduct(p: Product): T = m.fromProduct(p)
        override def keyToIndex(x: String): Int = macros.keyToIndex[T](x)
        override def allKeysArray = macros.fieldLabels[T].map(_._2).toArray
        override def storeDefaults(x: upickle.implicits.BaseCaseObjectContext): Unit = macros.storeDefaults[T](x)
      }

      inline if macros.isSingleton[T] then
        annotate[T](SingletonReader[T](macros.getSingleton[T]), macros.tagName[T])
      else if macros.isMemberOfSealedHierarchy[T] then
        annotate[T](reader, macros.tagName[T])
      else reader

    case m: Mirror.SumOf[T] =>
      implicit val currentlyDeriving: upickle.core.CurrentlyDeriving[T] = new upickle.core.CurrentlyDeriving()
      val readers: List[Reader[_ <: T]] = compiletime.summonAll[Tuple.Map[m.MirroredElemTypes, Reader]]
        .toList
        .asInstanceOf[List[Reader[_ <: T]]]

      Reader.merge[T](readers: _*)
  }

  inline def macroRAll[T](using m: Mirror.Of[T]): Reader[T] = inline m match {
    case m: Mirror.ProductOf[T] => macroR[T]
    case m: Mirror.SumOf[T] =>
      macros.defineEnumReaders[Reader[T], Tuple.Map[m.MirroredElemTypes, Reader]](this)
  }

  inline given superTypeReader[T: Mirror.ProductOf, V >: T : Reader]
                              (using NotGiven[CurrentlyDeriving[V]], NotGiven[T =:= V]): Reader[T] = {
    val actual = implicitly[Reader[V]].asInstanceOf[TaggedReader[T]]
    val tagName = macros.tagName[T]
    new TaggedReader.Leaf(tagName, actual.findReader(tagName))
  }

  // see comment in MacroImplicits as to why Dotty's extension methods aren't used here
  implicit class ReaderExtension(r: Reader.type):
    inline def derived[T](using Mirror.Of[T]): Reader[T] = macroRAll[T]
  end ReaderExtension
end ReadersVersionSpecific
