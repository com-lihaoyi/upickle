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

  final class Registry {
    private val inProgress = mutable.HashSet.empty[String]
    private val defs0 = mutable.LinkedHashMap.empty[String, ujson.Value]

    def ref(defKey: String): ujson.Obj = ujson.Obj("$ref" -> s"#/$$defs/$defKey")

    def define(defKey: String)(build: => ujson.Value): ujson.Obj = {
      if (defs0.contains(defKey) || inProgress.contains(defKey)) ref(defKey)
      else {
        inProgress += defKey
        defs0(defKey) = build
        inProgress -= defKey
        ref(defKey)
      }
    }

    def defs: collection.immutable.ListMap[String, ujson.Value] =
      collection.immutable.ListMap.from(defs0)
  }

  private def primitive(tpe: String): ujson.Obj = ujson.Obj("type" -> tpe)

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

  private inline def summonSumSchemas[T <: Tuple, Seen <: Tuple]: List[(Boolean, String, JsonSchema[Any])] =
    inline erasedValue[T] match {
      case _: EmptyTuple => Nil
      case _: (h *: t) =>
        (
          macros.isSingleton[h],
          macros.tagName[h],
          delayed(resolveSchema[h, Seen]).asInstanceOf[JsonSchema[Any]]
        ) :: summonSumSchemas[t, Seen]
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
    val fieldSchemas = summonSchemas[Elems, Seen]
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
            val mappedLabels = fieldLabels.map(api.objectAttributeKeyWriteMap(_).toString)
            val props = ujson.Obj.from(
              mappedLabels.zip(fieldSchemas).map { case (k, s) =>
                k -> s.schema(api, registry)
              }
            )
            ujson.Obj(
              "type" -> "object",
              "properties" -> props,
              "required" -> ujson.Arr(),
              "additionalProperties" -> fieldSchemas.nonEmpty
            )
          }
        }
      }
    }
  }

  private inline def sumSchema[T, Elems <: Tuple, Seen <: Tuple]: JsonSchema[T] = {
    val alts = summonSumSchemas[Elems, Seen]
    new JsonSchema[T] {
      override def schema(api: upickle.Api, registry: Registry): ujson.Value = {
        val defKey = typeId[T]
        registry.define(defKey) {
          val tagKey = api.tagName
          ujson.Obj(
            "oneOf" -> ujson.Arr.from(
              alts.map {
                case (true, tagName, _) =>
                  ujson.Obj("const" -> api.objectTypeKeyWriteMap(tagName).toString)
                case (false, tagName, altSchema) =>
                  ujson.Obj(
                    "allOf" -> ujson.Arr(
                      altSchema.schema(api, registry),
                      ujson.Obj(
                        "type" -> "object",
                        "properties" -> ujson.Obj(
                          tagKey -> ujson.Obj(
                            "const" -> api.objectTypeKeyWriteMap(tagName).toString
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
  given JsonSchema[Long] with { def schema(api: upickle.Api, registry: Registry) = primitive("integer") }
  given JsonSchema[Short] with { def schema(api: upickle.Api, registry: Registry) = primitive("integer") }
  given JsonSchema[Byte] with { def schema(api: upickle.Api, registry: Registry) = primitive("integer") }
  given JsonSchema[BigInt] with { def schema(api: upickle.Api, registry: Registry) = primitive("integer") }

  given JsonSchema[Double] with { def schema(api: upickle.Api, registry: Registry) = primitive("number") }
  given JsonSchema[Float] with { def schema(api: upickle.Api, registry: Registry) = primitive("number") }
  given JsonSchema[BigDecimal] with { def schema(api: upickle.Api, registry: Registry) = primitive("number") }

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
      ujson.Obj("type" -> "array", "items" -> inner.schema(api, registry))
  }
  given [T](using inner: JsonSchema[T]): JsonSchema[Vector[T]] = new JsonSchema[Vector[T]] {
    def schema(api: upickle.Api, registry: Registry): ujson.Value =
      ujson.Obj("type" -> "array", "items" -> inner.schema(api, registry))
  }
  given [T](using inner: JsonSchema[T]): JsonSchema[Seq[T]] = new JsonSchema[Seq[T]] {
    def schema(api: upickle.Api, registry: Registry): ujson.Value =
      ujson.Obj("type" -> "array", "items" -> inner.schema(api, registry))
  }
  given [T](using inner: JsonSchema[T]): JsonSchema[Set[T]] = new JsonSchema[Set[T]] {
    def schema(api: upickle.Api, registry: Registry): ujson.Value =
      ujson.Obj("type" -> "array", "items" -> inner.schema(api, registry), "uniqueItems" -> true)
  }
  given [T](using inner: JsonSchema[T]): JsonSchema[Array[T]] = new JsonSchema[Array[T]] {
    def schema(api: upickle.Api, registry: Registry): ujson.Value =
      ujson.Obj("type" -> "array", "items" -> inner.schema(api, registry))
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

  given [K, V](using valueSchema: JsonSchema[V]): JsonSchema[Map[K, V]] = new JsonSchema[Map[K, V]] {
    def schema(api: upickle.Api, registry: Registry): ujson.Value =
      ujson.Obj("type" -> "object", "additionalProperties" -> valueSchema.schema(api, registry))
  }
  given [K, V](using valueSchema: JsonSchema[V]): JsonSchema[scala.collection.mutable.LinkedHashMap[K, V]] =
    new JsonSchema[scala.collection.mutable.LinkedHashMap[K, V]] {
      def schema(api: upickle.Api, registry: Registry): ujson.Value =
        ujson.Obj("type" -> "object", "additionalProperties" -> valueSchema.schema(api, registry))
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
