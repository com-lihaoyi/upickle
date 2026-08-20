package upickle.jsonschema

import scala.collection.mutable
import scala.compiletime.{constValue, erasedValue, summonFrom}
import scala.deriving.Mirror
import scala.quoted.{Expr, Quotes, Type}
import upickle.implicits.macros

trait JsonSchema[+T] {
  def schema(api: upickle.Api, registry: JsonSchema.Registry): ujson.Value
}

object JsonSchema {
  private val Draft202012 = "https://json-schema.org/draft/2020-12/schema"
  private val IntegralPattern = "^-?(0|[1-9][0-9]*)$"
  private val BooleanPattern = "^(true|false)$"

  final class Registry {
    private val inProgress = mutable.HashSet.empty[String]
    private val defs0 = mutable.LinkedHashMap.empty[String, ujson.Value]

    def ref(defKey: String): ujson.Obj = ujson.Obj("$ref" -> s"#/$$defs/$defKey")

    def define(defKey: String)(build: => ujson.Value): ujson.Obj = {
      if (defs0.contains(defKey) || inProgress.contains(defKey)) ref(defKey)
      else {
        inProgress += defKey
        try defs0(defKey) = build
        finally inProgress -= defKey
        ref(defKey)
      }
    }

    def defs: collection.immutable.ListMap[String, ujson.Value] =
      collection.immutable.ListMap.from(defs0)
  }

  private def primitive(tpe: String): ujson.Obj = ujson.Obj("type" -> tpe)
  private def arraySchema(item: ujson.Value, uniqueItems: Boolean = false): ujson.Obj = {
    val out = ujson.Obj("type" -> "array", "items" -> item)
    if (uniqueItems) out("uniqueItems") = true
    out
  }

  trait MapKeySchema[-K] {
    def propertyNames: Option[ujson.Value]
  }
  given defaultMapKeySchema[K]: MapKeySchema[K] with {
    def propertyNames: Option[ujson.Value] = None
  }
  given MapKeySchema[java.util.UUID] with {
    def propertyNames: Option[ujson.Value] =
      Some(ujson.Obj("type" -> "string", "format" -> "uuid"))
  }
  given MapKeySchema[Char] with {
    def propertyNames: Option[ujson.Value] =
      Some(ujson.Obj("type" -> "string", "minLength" -> 1, "maxLength" -> 1))
  }
  given MapKeySchema[Boolean] with {
    def propertyNames: Option[ujson.Value] =
      Some(ujson.Obj("type" -> "string", "pattern" -> BooleanPattern))
  }
  given MapKeySchema[Int] with {
    def propertyNames: Option[ujson.Value] =
      Some(ujson.Obj("type" -> "string", "pattern" -> IntegralPattern))
  }
  given MapKeySchema[Long] with {
    def propertyNames: Option[ujson.Value] =
      Some(ujson.Obj("type" -> "string", "pattern" -> IntegralPattern))
  }
  given MapKeySchema[Short] with {
    def propertyNames: Option[ujson.Value] =
      Some(ujson.Obj("type" -> "string", "pattern" -> IntegralPattern))
  }
  given MapKeySchema[Byte] with {
    def propertyNames: Option[ujson.Value] =
      Some(ujson.Obj("type" -> "string", "pattern" -> IntegralPattern))
  }
  given MapKeySchema[BigInt] with {
    def propertyNames: Option[ujson.Value] =
      Some(ujson.Obj("type" -> "string", "pattern" -> IntegralPattern))
  }

  private def isTupleLabels(labels: List[String]): Boolean =
    labels.zipWithIndex.forall { case (label, index) => label == s"_${index + 1}" }

  private def isRef(v: ujson.Value): Boolean = v match {
    case o: ujson.Obj => o.value.size == 1 && o.obj.contains("$ref")
    case _ => false
  }

  private inline def typeId[T]: String = ${typeIdImpl[T]}
  private def typeIdImpl[T](using q: Quotes, t: Type[T]): Expr[String] = {
    import q.reflect._
    Expr(TypeRepr.of[T].show)
  }

  private inline def labelsToList[T <: Tuple]: List[String] = inline erasedValue[T] match {
    case _: EmptyTuple => Nil
    case _: (h *: t) => constValue[h].toString :: labelsToList[t]
  }

  private inline def productFieldMetadata[T]: List[(String, Boolean, Boolean, Boolean, String)] = ${productFieldMetadataImpl[T]}
  private def productFieldMetadataImpl[T](using q: Quotes, t: Type[T]): Expr[List[(String, Boolean, Boolean, Boolean, String)]] = {
    import q.reflect.*
    def keyAnnotation(sym: Symbol): Option[String] =
      sym.annotations.collectFirst {
        case Apply(Select(New(tpt), _), List(Literal(StringConstant(s))))
            if tpt.tpe =:= TypeRepr.of[upickle.implicits.key] =>
          s
      }
    def flattenAnnotation(sym: Symbol): Boolean =
      sym.annotations.exists(_.tpe =:= TypeRepr.of[upickle.implicits.flatten])

    def substituteTypeArgs(owner: TypeRepr, fieldType: TypeRepr): TypeRepr = {
      val constructorSym = owner.typeSymbol.primaryConstructor
      val tparams0 = constructorSym.paramSymss.flatten.filter(_.isType)
      fieldType.substituteTypes(tparams0, owner.typeArgs)
    }

    def isCollectionFlattenable(tpe: TypeRepr): Boolean = {
      val iterableSym = Symbol.requiredClass("scala.collection.Iterable")
      tpe.baseType(iterableSym) match {
        case AppliedType(_, List(elemTpe)) =>
          elemTpe.dealias match {
            case AppliedType(tupleConstructor, List(_, _)) =>
              tupleConstructor.typeSymbol == Symbol.requiredClass("scala.Tuple2")
            case _ => false
          }
        case _ => false
      }
    }

    def mapKeyKindForType(tpe: TypeRepr): String = {
      val d = tpe.dealias
      if (d =:= TypeRepr.of[Int] || d =:= TypeRepr.of[Long] || d =:= TypeRepr.of[Short] || d =:= TypeRepr.of[Byte] || d =:= TypeRepr.of[BigInt]) "integral"
      else if (d =:= TypeRepr.of[Boolean]) "boolean"
      else if (d =:= TypeRepr.of[Char]) "char"
      else if (d =:= TypeRepr.of[java.util.UUID]) "uuid"
      else "none"
    }

    def flattenCollectionKeyKind(tpe: TypeRepr): String = {
      val iterableSym = Symbol.requiredClass("scala.collection.Iterable")
      tpe.baseType(iterableSym) match {
        case AppliedType(_, List(elemTpe)) =>
          elemTpe.dealias match {
            case AppliedType(tupleConstructor, List(keyTpe, _))
                if tupleConstructor.typeSymbol == Symbol.requiredClass("scala.Tuple2") =>
              mapKeyKindForType(keyTpe)
            case _ => "none"
          }
        case _ => "none"
      }
    }

    val owner = TypeRepr.of[T]
    val fields = owner.typeSymbol.primaryConstructor.paramSymss.flatten.filterNot(_.isType)
    Expr.ofList(fields.map { f =>
      val mapped = keyAnnotation(f).getOrElse(f.name)
      val isFlatten = flattenAnnotation(f)
      val fieldTpe = substituteTypeArgs(owner, owner.memberType(f))
      val isFlattenMap = isFlatten && isCollectionFlattenable(fieldTpe)
      val keyKind = if (isFlattenMap) flattenCollectionKeyKind(fieldTpe) else "none"
      Expr((mapped, f.flags.is(Flags.HasDefault), isFlatten, isFlattenMap, keyKind))
    })
  }

  private inline def containsType[T, Ts <: Tuple]: Boolean =
    inline erasedValue[Ts] match {
      case _: EmptyTuple => false
      case _: (T *: t) => true
      case _: (_ *: t) => containsType[T, t]
    }

  private inline def refSchema[T]: JsonSchema[T] = new JsonSchema[T] {
    def schema(api: upickle.Api, registry: Registry): ujson.Value = registry.ref(typeId[T])
  }

  private inline def resolveSchema[T, Seen <: Tuple]: JsonSchema[T] =
    inline if containsType[T, Seen] then refSchema[T]
    else summonFrom {
      case s: JsonSchema[T] => s
      case m: Mirror.Of[T] => derivedWithSeen[T, Seen](using m)
    }

  private def delayed[T](f: => JsonSchema[T]): JsonSchema[T] = new JsonSchema[T] {
    lazy val value = f
    def schema(api: upickle.Api, registry: Registry): ujson.Value = value.schema(api, registry)
  }

  private inline def summonSchemas[T <: Tuple, Seen <: Tuple]: List[JsonSchema[Any]] = inline erasedValue[T] match {
    case _: EmptyTuple => Nil
    case _: (h *: t) =>
      delayed(resolveSchema[h, Seen]).asInstanceOf[JsonSchema[Any]] :: summonSchemas[t, Seen]
  }

  private inline def summonSumSchemas[T <: Tuple, Seen <: Tuple]: List[(Boolean, String, String, JsonSchema[Any])] =
    inline erasedValue[T] match {
      case _: EmptyTuple => Nil
      case _: (h *: t) =>
        (
          macros.isSingleton[h],
          macros.tagName[h],
          macros.shortTagName[h],
          delayed(resolveSchema[h, Seen]).asInstanceOf[JsonSchema[Any]]
        ) :: summonSumSchemas[t, Seen]
    }

  private inline def annotatedSumTagKey[T]: Option[String] = ${annotatedSumTagKeyImpl[T]}
  private def annotatedSumTagKeyImpl[T](using q: Quotes, t: Type[T]): Expr[Option[String]] = {
    import q.reflect.*
    TypeRepr.of[T].typeSymbol.annotations.collectFirst {
      case Apply(Select(New(tpt), _), List(Literal(StringConstant(s))))
          if tpt.tpe =:= TypeRepr.of[upickle.implicits.key] =>
        Expr(s)
    } match {
      case Some(v) => '{Some($v)}
      case None => '{None}
    }
  }

  private inline def annotatedAllowUnknownKeys[T]: Option[Boolean] = ${annotatedAllowUnknownKeysImpl[T]}
  private def annotatedAllowUnknownKeysImpl[T](using q: Quotes, t: Type[T]): Expr[Option[Boolean]] = {
    import q.reflect.*
    TypeRepr.of[T].typeSymbol.annotations.collectFirst {
      case Apply(Select(New(tpt), _), List(Literal(BooleanConstant(b))))
          if tpt.tpe =:= TypeRepr.of[upickle.implicits.allowUnknownKeys] =>
        Expr(b)
    } match {
      case Some(v) => '{Some($v)}
      case None => '{None}
    }
  }

  private def derefSchema(schema: ujson.Value, registry: Registry): Option[ujson.Obj] = schema match {
    case o: ujson.Obj =>
      o.obj.get("$ref") match {
        case Some(ujson.Str(ref)) if ref.startsWith("#/$defs/") =>
          registry.defs.get(ref.stripPrefix("#/$defs/")).collect { case obj: ujson.Obj => obj }
        case _ => Some(o)
      }
    case _ => None
  }

  private def mapSchema(valueSchema: ujson.Value, keySchema: MapKeySchema[?]): ujson.Obj = {
    val out = ujson.Obj("type" -> "object", "additionalProperties" -> valueSchema)
    keySchema.propertyNames.foreach(v => out("propertyNames") = v)
    out
  }

  private def propertyNamesFromKeyKind(kind: String): Option[ujson.Value] = kind match {
    case "integral" => Some(ujson.Obj("type" -> "string", "pattern" -> IntegralPattern))
    case "boolean" => Some(ujson.Obj("type" -> "string", "pattern" -> BooleanPattern))
    case "char" => Some(ujson.Obj("type" -> "string", "minLength" -> 1, "maxLength" -> 1))
    case "uuid" => Some(ujson.Obj("type" -> "string", "format" -> "uuid"))
    case _ => None
  }

  private def flattenMapValueSchema(schema: ujson.Value, registry: Registry): Option[ujson.Value] = {
    derefSchema(schema, registry) match {
      case Some(obj) if obj.obj.contains("additionalProperties") =>
        obj.obj.get("additionalProperties")
      case Some(obj) if obj.obj.get("type").contains(ujson.Str("array")) =>
        obj.obj.get("items") match {
          case Some(itemObj: ujson.Obj) =>
            itemObj.obj.get("prefixItems") match {
              case Some(prefixItems: ujson.Arr) if prefixItems.value.length >= 2 =>
                val valueSchema = prefixItems.value(1)
                Some(valueSchema)
              case _ => None
            }
          case _ => None
        }
      case _ => None
    }
  }


  inline def derived[T](using m: Mirror.Of[T]): JsonSchema[T] =
    derivedWithSeen[T, EmptyTuple](using m)

  private inline def derivedWithSeen[T, Seen <: Tuple](using m: Mirror.Of[T]): JsonSchema[T] =
    inline m match {
      case _: Mirror.ProductOf[T] => productSchema[T, m.MirroredElemLabels, m.MirroredElemTypes, T *: Seen]
      case _: Mirror.SumOf[T] => sumSchema[T, m.MirroredElemTypes, T *: Seen]
    }

  transparent inline given product[T](using m: Mirror.ProductOf[T]): JsonSchema[T] =
    productSchema[T, m.MirroredElemLabels, m.MirroredElemTypes, T *: EmptyTuple]

  transparent inline given sum[T](using m: Mirror.SumOf[T]): JsonSchema[T] =
    sumSchema[T, m.MirroredElemTypes, T *: EmptyTuple]

  private inline def productSchema[T, Labels <: Tuple, Elems <: Tuple, Seen <: Tuple]: JsonSchema[T] = {
    val fieldLabels = labelsToList[Labels]
    val rawFieldMeta = productFieldMetadata[T]
    val fieldSchemas = summonSchemas[Elems, Seen]
    val objectAllowUnknownOverride = annotatedAllowUnknownKeys[T]
    new JsonSchema[T] {
      override def schema(api: upickle.Api, registry: Registry): ujson.Value = {
        val defKey = typeId[T]
        registry.define(defKey) {
          if (fieldLabels.nonEmpty && isTupleLabels(fieldLabels)) {
            ujson.Obj(
              "type" -> "array",
              "prefixItems" -> ujson.Arr.from(fieldSchemas.map(_.schema(api, registry))),
              "minItems" -> fieldSchemas.size,
              "maxItems" -> fieldSchemas.size
            )
          } else {
            val chosenLabels =
              if (rawFieldMeta.size == fieldSchemas.size) rawFieldMeta.map(_._1)
              else fieldLabels
            val fieldMetaByLabel = rawFieldMeta.map { case (label, hasDefault, isFlatten, isFlattenMap, keyKind) =>
              label -> (hasDefault, isFlatten, isFlattenMap, keyKind)
            }.toMap
            val allowUnknown = objectAllowUnknownOverride.getOrElse(api.allowUnknownKeys)
            val props = ujson.Obj()
            val required = mutable.LinkedHashSet.empty[String]
            var flattenMapAdditionalProperties: Option[ujson.Value] = None
            var flattenMapPropertyNames: Option[ujson.Value] = None

            chosenLabels.zip(fieldSchemas).foreach { case (label, schema) =>
              val mappedLabel = api.objectAttributeKeyWriteMap(label).toString
              fieldMetaByLabel.get(label) match {
                case Some((_, _, true, keyKind)) =>
                  flattenMapValueSchema(schema.schema(api, registry), registry)
                    .foreach(v => flattenMapAdditionalProperties = Some(v))
                  propertyNamesFromKeyKind(keyKind).foreach(v => flattenMapPropertyNames = Some(v))
                case Some((_, true, _, _)) =>
                  val nestedObj = derefSchema(schema.schema(api, registry), registry)
                  nestedObj match {
                    case Some(obj) =>
                      obj.obj.get("properties").collect { case p: ujson.Obj => p }.foreach { p =>
                        p.value.foreach { case (k, v) => props(k) = v }
                      }
                      obj.obj.get("required").collect { case arr: ujson.Arr => arr }.foreach { arr =>
                        arr.value.foreach {
                          case ujson.Str(k) => required += k
                          case _ =>
                        }
                      }
                    case None =>
                      props(mappedLabel) = schema.schema(api, registry)
                  }
                case Some((hasDefault, _, _, _)) =>
                  props(mappedLabel) = schema.schema(api, registry)
                  if (!hasDefault) required += mappedLabel
                case None =>
                  props(mappedLabel) = schema.schema(api, registry)
                  required += mappedLabel
              }
            }

            val additionalProperties = flattenMapAdditionalProperties.getOrElse(ujson.Bool(allowUnknown))
            val out = ujson.Obj(
              "type" -> "object",
              "properties" -> props,
              "required" -> ujson.Arr.from(required),
              "additionalProperties" -> additionalProperties
            )
            flattenMapPropertyNames.foreach { v =>
              if (props.value.isEmpty) out("propertyNames") = v
              else {
                out("propertyNames") = ujson.Obj(
                  "anyOf" -> ujson.Arr(
                    v,
                    ujson.Obj("enum" -> ujson.Arr.from(props.value.keys))
                  )
                )
              }
            }
            out
          }
        }
      }
    }
  }

  private inline def sumSchema[T, Elems <: Tuple, Seen <: Tuple]: JsonSchema[T] = {
    val alts = summonSumSchemas[Elems, Seen]
    val tagKeyOverride = annotatedSumTagKey[T]
    new JsonSchema[T] {
      override def schema(api: upickle.Api, registry: Registry): ujson.Value = {
        val defKey = typeId[T]
        registry.define(defKey) {
          val tagKey = api.objectAttributeKeyWriteMap(tagKeyOverride.getOrElse(api.tagName)).toString
          ujson.Obj(
            "oneOf" -> ujson.Arr.from(
              alts.map {
                case (true, fullTagName, shortTagName, _) =>
                  val rawTag = if (api.objectTypeKeyWriteFullyQualified) fullTagName else shortTagName
                  ujson.Obj("const" -> api.objectTypeKeyWriteMap(rawTag).toString)
                case (false, fullTagName, shortTagName, altSchema) =>
                  val rawTag = if (api.objectTypeKeyWriteFullyQualified) fullTagName else shortTagName
                  ujson.Obj(
                    "allOf" -> ujson.Arr(
                      altSchema.schema(api, registry),
                      ujson.Obj(
                        "type" -> "object",
                        "properties" -> ujson.Obj(
                          tagKey -> ujson.Obj(
                            "const" -> api.objectTypeKeyWriteMap(rawTag).toString
                          )
                        ),
                        "required" -> ujson.Arr(tagKey)
                      )
                    )
                  )
              }
            )
          )
        }
      }
    }
  }

  given JsonSchema[String] with { def schema(api: upickle.Api, registry: Registry) = primitive("string") }
  given JsonSchema[Char] with { def schema(api: upickle.Api, registry: Registry) = primitive("string") }
  given JsonSchema[Symbol] with { def schema(api: upickle.Api, registry: Registry) = primitive("string") }
  given JsonSchema[java.util.UUID] with { def schema(api: upickle.Api, registry: Registry) = primitive("string") }
  given JsonSchema[Boolean] with { def schema(api: upickle.Api, registry: Registry) = primitive("boolean") }

  given JsonSchema[Int] with { def schema(api: upickle.Api, registry: Registry) = primitive("integer") }
  given JsonSchema[Long] with {
    def schema(api: upickle.Api, registry: Registry) =
      ujson.Obj(
        "anyOf" -> ujson.Arr(
          primitive("integer"),
          ujson.Obj("type" -> "string", "pattern" -> IntegralPattern)
        )
      )
  }
  given JsonSchema[Short] with { def schema(api: upickle.Api, registry: Registry) = primitive("integer") }
  given JsonSchema[Byte] with { def schema(api: upickle.Api, registry: Registry) = primitive("integer") }
  given JsonSchema[BigInt] with {
    def schema(api: upickle.Api, registry: Registry) =
      ujson.Obj("type" -> "string", "pattern" -> IntegralPattern)
  }

  given JsonSchema[Double] with { def schema(api: upickle.Api, registry: Registry) = primitive("number") }
  given JsonSchema[Float] with { def schema(api: upickle.Api, registry: Registry) = primitive("number") }
  given JsonSchema[BigDecimal] with { def schema(api: upickle.Api, registry: Registry) = primitive("string") }

  given JsonSchema[Unit] with { def schema(api: upickle.Api, registry: Registry) = ujson.Obj("type" -> "null") }
  given JsonSchema[ujson.Value] with { def schema(api: upickle.Api, registry: Registry) = ujson.Obj() }

  given [T](using inner: JsonSchema[T]): JsonSchema[Option[T]] = new JsonSchema[Option[T]] {
    def schema(api: upickle.Api, registry: Registry): ujson.Value =
      if (api.optionsAsNulls) {
        ujson.Obj(
          "anyOf" -> ujson.Arr(
            inner.schema(api, registry),
            ujson.Obj("type" -> "null"),
            ujson.Obj(
              "type" -> "array",
              "minItems" -> 0,
              "maxItems" -> 1,
              "items" -> inner.schema(api, registry)
            )
          )
        )
      } else {
        ujson.Obj(
          "type" -> "array",
          "minItems" -> 0,
          "maxItems" -> 1,
          "items" -> inner.schema(api, registry)
        )
      }
  }

  given [T](using inner: JsonSchema[T]): JsonSchema[List[T]] = new JsonSchema[List[T]] {
    def schema(api: upickle.Api, registry: Registry): ujson.Value =
      arraySchema(inner.schema(api, registry))
  }
  given [T](using inner: JsonSchema[T]): JsonSchema[Vector[T]] = new JsonSchema[Vector[T]] {
    def schema(api: upickle.Api, registry: Registry): ujson.Value =
      arraySchema(inner.schema(api, registry))
  }
  given [T](using inner: JsonSchema[T]): JsonSchema[Seq[T]] = new JsonSchema[Seq[T]] {
    def schema(api: upickle.Api, registry: Registry): ujson.Value =
      arraySchema(inner.schema(api, registry))
  }
  given [T](using inner: JsonSchema[T]): JsonSchema[Set[T]] = new JsonSchema[Set[T]] {
    def schema(api: upickle.Api, registry: Registry): ujson.Value =
      arraySchema(inner.schema(api, registry), uniqueItems = true)
  }
  given [T](using inner: JsonSchema[T]): JsonSchema[Array[T]] = new JsonSchema[Array[T]] {
    def schema(api: upickle.Api, registry: Registry): ujson.Value =
      arraySchema(inner.schema(api, registry))
  }
  given [A, B](using aSchema: JsonSchema[A], bSchema: JsonSchema[B]): JsonSchema[(A, B)] =
    new JsonSchema[(A, B)] {
      override def schema(api: upickle.Api, registry: Registry): ujson.Value = {
        ujson.Obj(
          "type" -> "array",
          "prefixItems" -> ujson.Arr(
            aSchema.schema(api, registry),
            bSchema.schema(api, registry)
          ),
          "minItems" -> 2,
          "maxItems" -> 2
        )
      }
    }

  given [K, V](using valueSchema: JsonSchema[V], keySchema: MapKeySchema[K]): JsonSchema[Map[K, V]] =
    new JsonSchema[Map[K, V]] {
      def schema(api: upickle.Api, registry: Registry): ujson.Value =
        mapSchema(valueSchema.schema(api, registry), keySchema)
    }
  given [K, V](
      using valueSchema: JsonSchema[V],
      keySchema: MapKeySchema[K]
  ): JsonSchema[scala.collection.mutable.LinkedHashMap[K, V]] =
    new JsonSchema[scala.collection.mutable.LinkedHashMap[K, V]] {
      def schema(api: upickle.Api, registry: Registry): ujson.Value =
        mapSchema(valueSchema.schema(api, registry), keySchema)
    }

  def schemaFor[T](api: upickle.Api)(using JsonSchema[T]): ujson.Value = {
    val registry = new Registry
    val root = summon[JsonSchema[T]].schema(api, registry)
    val defs = registry.defs
    if (defs.isEmpty) {
      root match {
        case obj: ujson.Obj =>
          obj("$schema") = ujson.Str(Draft202012)
          obj
        case other =>
          ujson.Obj("$schema" -> Draft202012, "allOf" -> ujson.Arr(other))
      }
    } else {
      val out = ujson.Obj(
        "$schema" -> Draft202012,
        "$defs" -> ujson.Obj.from(defs)
      )
      if (isRef(root)) out("$ref") = root("$ref")
      else out("allOf") = ujson.Arr(root)
      out
    }
  }

  def definitionsFor[T](api: upickle.Api)(using JsonSchema[T]): collection.immutable.ListMap[String, ujson.Value] = {
    val registry = new Registry
    summon[JsonSchema[T]].schema(api, registry)
    registry.defs
  }
}

extension (api: upickle.Api) {
  inline def schema[T](using api.ReadWriter[T], JsonSchema[T]): ujson.Value =
    JsonSchema.schemaFor[T](api)

  inline def schemas[T](using api.ReadWriter[T], JsonSchema[T]): collection.immutable.ListMap[String, ujson.Value] =
    JsonSchema.definitionsFor[T](api)
}
