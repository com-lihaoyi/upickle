package upickle.implicits

import compiletime.summonInline
import deriving.Mirror
import scala.util.NotGiven
import upickle.core.{Annotator, ObjVisitor, Visitor, Abort, CurrentlyDeriving}
import upickle.implicits.BaseCaseObjectContext
import scala.collection.mutable

trait ReadersVersionSpecific
  extends MacrosCommon
    with upickle.core.Types
    with Annotator
    with CaseClassReadWriters:

  private[upickle] abstract class CaseClassReader3V3[T](paramCount: Int,
                                     missingKeyCount: Long,
                                     allowUnknownKeys: Boolean,
                                     construct: (Array[Any], scala.collection.mutable.ListBuffer[(Any, Any)]) => T) extends CaseClassReader[T] {

    def visitors0: ((AnyRef, AnyRef), Array[AnyRef])
    lazy val ((visitorMapKey, visitorMapValue), visitors) = visitors0
    lazy val hasFlattenOnMap = visitorMapValue ne null
    def keyToIndex(x: String): Int
    def allKeysArray: Array[String]
    def storeDefaults(x: upickle.implicits.BaseCaseObjectContext2): Unit
    trait ObjectContext extends ObjVisitor[Any, T] with BaseCaseObjectContext2 {
      private val params = new Array[Any](paramCount)
      private val collection = scala.collection.mutable.ListBuffer.empty[(Any, Any)]
      private var currentKey: Any = null
      protected var storeToMap = false

      override def visitKey(index: Int): Visitor[_, _] =
        if (hasFlattenOnMap) upickle.core.BufferedValue.Builder
        else upickle.core.StringVisitor

      def storeAggregatedValue(currentIndex: Int, v: Any): Unit =
        if (currentIndex == -1) {
          if (storeToMap) {
            collection += (currentKey -> v)
          }
        } else {
          params(currentIndex) = v
        }

      def subVisitor: Visitor[_, _] =
        if (currentIndex == -1) {
          if (hasFlattenOnMap) visitorMapValue.asInstanceOf[Visitor[_, _]]
          else upickle.core.NoOpVisitor
        }
        else {
          visitors(currentIndex).asInstanceOf[Visitor[_, _]]
        }

      def visitKeyValue(v: Any): Unit =
        storeToMap = false
        val currentKeyString =
          if (hasFlattenOnMap) objectAttributeKeyReadMap(upickle.core.BufferedValue.valueToSortKey(v.asInstanceOf[upickle.core.BufferedValue])).toString
          else objectAttributeKeyReadMap(v.toString).toString
        currentIndex = keyToIndex(currentKeyString)
        if (currentIndex == -1) {
          if (hasFlattenOnMap) {
            // Convert the BufferedValue key to the key type using the key reader
            currentKey = upickle.core.BufferedValue.transform(v.asInstanceOf[upickle.core.BufferedValue], visitorMapKey.asInstanceOf[Visitor[_, _]])
            storeToMap = true
          } else if (!allowUnknownKeys) {
            throw new upickle.core.Abort("Unknown Key: " + currentKeyString)
          }
        }

      def visitEnd(index: Int): T =
        storeDefaults(this)

        // Special-case 64 because java bit shifting ignores any RHS values above 63
        // https://docs.oracle.com/javase/specs/jls/se7/html/jls-15.html#jls-15.19
        if (this.checkErrorMissingKeys(missingKeyCount))
          this.errorMissingKeys(paramCount, allKeysArray)

        construct(params, collection)
    }

    override def visitObject(length: Int,
                             jsonableKeys: Boolean,
                             index: Int) =
      if (paramCount <= 64) new CaseObjectContext2[T](paramCount) with ObjectContext {
        override def visitValue(v: Any, index: Int): Unit = {
          if ((currentIndex != -1) && ((found & (1L << currentIndex)) == 0)) {
            storeAggregatedValue(currentIndex, v)
            found |= (1L << currentIndex)
          }
          else if (storeToMap) {
            storeAggregatedValue(currentIndex, v)
          }
        }
      }
      else new HugeCaseObjectContext2[T](paramCount) with ObjectContext {
        override def visitValue(v: Any, index: Int): Unit = {
          if ((currentIndex != -1) && ((found(currentIndex / 64) & (1L << currentIndex)) == 0)) {
            storeAggregatedValue(currentIndex, v)
            found(currentIndex / 64) |= (1L << currentIndex)
          }
          else if (storeToMap) {
            storeAggregatedValue(currentIndex, v)
          }
        }
      }
  }

  @deprecated
  abstract class CaseClassReader3V2[T](paramCount: Int,
                                     missingKeyCount: Long,
                                     allowUnknownKeys: Boolean,
                                     construct: (Array[Any], scala.collection.mutable.ListBuffer[(String, Any)]) => T) extends CaseClassReader[T] {

    def visitors0: (AnyRef, Array[AnyRef])
    lazy val (visitorMap, visitors) = visitors0
    lazy val hasFlattenOnMap = visitorMap ne null
    def keyToIndex(x: String): Int
    def allKeysArray: Array[String]
    def storeDefaults(x: upickle.implicits.BaseCaseObjectContext): Unit
    trait ObjectContext extends ObjVisitor[Any, T] with BaseCaseObjectContext {
      private val params = new Array[Any](paramCount)
      private val collection = scala.collection.mutable.ListBuffer.empty[(String, Any)]
      private var currentKey = ""
      protected var storeToMap = false

      def storeAggregatedValue(currentIndex: Int, v: Any): Unit = 
        if (currentIndex == -1) {
          if (storeToMap) {
            collection += (currentKey -> v)
          }
        } else {
          params(currentIndex) = v
        }

      def subVisitor: Visitor[_, _] =
        if (currentIndex == -1) {
          if (hasFlattenOnMap) visitorMap.asInstanceOf[Visitor[_, _]]
          else upickle.core.NoOpVisitor
        } 
        else {
          visitors(currentIndex).asInstanceOf[Visitor[_, _]]
        }

      def visitKeyValue(v: Any): Unit =
        storeToMap = false
        currentKey = objectAttributeKeyReadMap(v.toString).toString
        currentIndex = keyToIndex(currentKey)
        if (currentIndex == -1) {
          if (hasFlattenOnMap) {
            storeToMap = true
          } else if (!allowUnknownKeys) {
            throw new upickle.core.Abort("Unknown Key: " + currentKey)
          }
        }

      def visitEnd(index: Int): T =
        storeDefaults(this)

        // Special-case 64 because java bit shifting ignores any RHS values above 63
        // https://docs.oracle.com/javase/specs/jls/se7/html/jls-15.html#jls-15.19
        if (this.checkErrorMissingKeys(missingKeyCount))
          this.errorMissingKeys(paramCount, allKeysArray)

        construct(params, collection)
    }

    override def visitObject(length: Int,
                             jsonableKeys: Boolean,
                             index: Int) =
      if (paramCount <= 64) new CaseObjectContext[T](paramCount) with ObjectContext {
        override def visitValue(v: Any, index: Int): Unit = {
          if ((currentIndex != -1) && ((found & (1L << currentIndex)) == 0)) {
            storeAggregatedValue(currentIndex, v)
            found |= (1L << currentIndex)
          }
          else if (storeToMap) {
            storeAggregatedValue(currentIndex, v)
          }
        }
      }
      else new HugeCaseObjectContext[T](paramCount) with ObjectContext {
        override def visitValue(v: Any, index: Int): Unit = {
          if ((currentIndex != -1) && ((found(currentIndex / 64) & (1L << currentIndex)) == 0)) {
            storeAggregatedValue(currentIndex, v)
            found(currentIndex / 64) |= (1L << currentIndex)
          }
          else if (storeToMap) {
            storeAggregatedValue(currentIndex, v)
          }
        }
      }
  }

  inline def macroR[T](using m: Mirror.Of[T]): Reader[T] = inline m match {
    case m: Mirror.ProductOf[T] =>
      macros.validateFlattenAnnotation[T]()
      val paramCount = macros.paramsCount[T]
      val reader = new CaseClassReader3V3[T](
        paramCount,
        if (paramCount <= 64) if (paramCount == 64) -1 else (1L << paramCount) - 1
        else paramCount,
        macros.extractIgnoreUnknownKeys[T]().headOption.getOrElse(this.allowUnknownKeys),
        (params: Array[Any], collection: scala.collection.mutable.ListBuffer[(Any, Any)]) => macros.applyConstructor[T](params, collection)
      ){
        override def visitors0 = macros.allReaders[T, Reader]
        override def keyToIndex(x: String): Int = macros.keyToIndex[T](x)
        override def allKeysArray = macros.allFieldsMappedName[T].toArray
        override def storeDefaults(x: upickle.implicits.BaseCaseObjectContext2): Unit = macros.storeDefaults[T](x)
      }

      inline if macros.isSingleton[T] then
        annotate[T](
          SingletonReader[T](macros.getSingleton[T]),
          macros.tagKey[T](outerThis),
          macros.tagName[T],
          macros.shortTagName[T]
        )
      else if macros.isMemberOfSealedHierarchy[T] then
        annotate[T](
          reader,
          macros.tagKey[T](outerThis),
          macros.tagName[T],
          macros.shortTagName[T],
        )
      else reader

    case m: Mirror.SumOf[T] =>
      implicit val currentlyDeriving: upickle.core.CurrentlyDeriving[T] = new upickle.core.CurrentlyDeriving()
      val readers: List[Reader[_ <: T]] = compiletime.summonAll[Tuple.Map[m.MirroredElemTypes, Reader]]
        .toList
        .asInstanceOf[List[Reader[_ <: T]]]

      Reader.merge[T](macros.tagKey[T](outerThis), readers: _*)
  }

  inline def macroRAll[T](using m: Mirror.Of[T]): Reader[T] = inline m match {
    case m: Mirror.ProductOf[T] => macroR[T]
    case m: Mirror.SumOf[T] =>
      macros.defineEnumReaders[Reader[T], Tuple.Map[m.MirroredElemTypes, Reader]](this)
  }

  inline given superTypeReader[T: Mirror.ProductOf, V >: T : Reader : Mirror.SumOf]
                              (using NotGiven[CurrentlyDeriving[V]]): Reader[T] = {
    val actual = implicitly[Reader[V]].asInstanceOf[TaggedReader[T]]
    val tagKey = macros.tagKey[T](outerThis)
    val tagName = macros.tagName[T]
    val shortTagName = macros.shortTagName[T]
    new TaggedReader.Leaf(tagKey, tagName, shortTagName, actual.findReader(tagName))
  }

  // see comment in MacroImplicits as to why Dotty's extension methods aren't used here
  implicit class ReaderExtension(r: Reader.type):
    inline def derived[T](using Mirror.Of[T]): Reader[T] = macroRAll[T]
  end ReaderExtension

  @deprecated
  abstract class CaseClassReader3[T](paramCount: Int,
                                     missingKeyCount: Long,
                                     allowUnknownKeys: Boolean,
                                     construct: Array[Any] => T) extends CaseClassReader[T] {
    def visitors0: Product
    lazy val visitors = visitors0
    def fromProduct(p: Product): T
    def keyToIndex(x: String): Int
    def allKeysArray: Array[String]
    def storeDefaults(x: upickle.implicits.BaseCaseObjectContext): Unit

    trait ObjectContext extends ObjVisitor[Any, T] with BaseCaseObjectContext {
      private val params = new Array[Any](paramCount)

      def storeAggregatedValue(currentIndex: Int, v: Any): Unit = params(currentIndex) = v

      def subVisitor: Visitor[_, _] =
        if (currentIndex == -1) upickle.core.NoOpVisitor
        else visitors.productElement(currentIndex).asInstanceOf[Visitor[_, _]]

      def visitKeyValue(v: Any): Unit =
        val k = objectAttributeKeyReadMap(v.toString).toString
        currentIndex = keyToIndex(k)
        if (currentIndex == -1 && !allowUnknownKeys) {
          throw new upickle.core.Abort("Unknown Key: " + k.toString)
        }

      def visitEnd(index: Int): T =
        storeDefaults(this)

        // Special-case 64 because java bit shifting ignores any RHS values above 63
        // https://docs.oracle.com/javase/specs/jls/se7/html/jls-15.html#jls-15.19
        if (this.checkErrorMissingKeys(missingKeyCount))
          this.errorMissingKeys(paramCount, allKeysArray)

        construct(params)
    }

    override def visitObject(length: Int,
                             jsonableKeys: Boolean,
                             index: Int) =
      if (paramCount <= 64) new CaseObjectContext[T](paramCount) with ObjectContext
      else new HugeCaseObjectContext[T](paramCount) with ObjectContext
  }


end ReadersVersionSpecific
