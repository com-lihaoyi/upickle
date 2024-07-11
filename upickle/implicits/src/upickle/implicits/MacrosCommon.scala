package upickle.implicits

// Common things for derivation
trait MacrosCommon {
  /**
   * Whether to use the fully-qualified name of `case class`es and `case object`s which
   * are part of `sealed trait` hierarchies when serializing them and writing their `$type`
   * key. Defaults to `false`, so `$type` key uses the shortest partially-qualified name.
   * Can be set to `true` to use their fully-qualified name.
   */
  def objectTypeKeyWriteFullyQualified: Boolean = false

  /**
   * Whether or not to write `case class` keys which match their default values.
   * Defaults to `false`, allowing those keys to be omitted. Can be set to `true`
   * to always write field values even if they are equal to the default
   */
  def serializeDefaults: Boolean = false

  /**
   * Transform dictionary keys when writing `case class`es when reading. Can
   * be overriden to provide custom mappings between Scala field names and JSON
   * field names. Needs to be kept in sync with [[objectAttributeKeyWriteMap]]
   *
   * This customizes the mapping across all `case class`es fields handled by this
   * upickle instance. This can be customized on a field-by-field basis using the
   * [[upickle.implicits.key]] annotation on the `case class` field
   */
  def objectAttributeKeyReadMap(s: CharSequence): CharSequence = s

  /**
   * Map the name of JSON object fields to Scala `case class` fields during serialization.
   * Must be kept in sync with [[objectAttributeKeyReadMap]]
   */
  def objectAttributeKeyWriteMap(s: CharSequence): CharSequence = s

  /**
   * Transforms the value of the `$type` field when writing `sealed trait`s,
   * to allow custom mapping between the `case class` name and the `$type` field
   * in the generated JSON. Must be kept in sync with [[objectTypeKeyWriteMap]].
   *
   * * This customizes the mapping across all `case class`es fields handled by this
   * * upickle instance. This can be customized on a per-`sealed trait` basis using the
   * * [[upickle.implicits.key]] annotation on the `case class`
   */
  def objectTypeKeyReadMap(s: CharSequence): CharSequence = s

  /**
   * Map the name of Scala `case class` type names to JSON `$type` field value during
   * serialization. Must be kept in sync with [[objectTypeKeyReadMap]]
   */
  def objectTypeKeyWriteMap(s: CharSequence): CharSequence = s

  /**
   * Whether top-level `Some(t)`s and `None`s are serialized unboxed as `t` or
   * `null`, rather than `[t]` or `[]`. This is generally what people expect,
   * although it does cause issues where `Some(null)` when serialized and de-serialized
   * can become `None`. Can be disabled to use the boxed serialization format
   * as 0-or-1-element-arrays, presering round trip-ability at the expense of
   * un-intuitiveness and verbosity
   */
  def optionsAsNulls: Boolean = true
}

object MacrosCommon {
  def tagKeyFromParents[P](
    typeName: => String,
    sealedParents: List[P],
    getKey: P => Option[String],
    getName: P => String,
    fail: String => Nothing,
  ): String =
    /**
      * Valid cases are:
      *
      * 1. None of the parents have a `@key` annotation
      * 2. All of the parents have the same `@key` annotation
      */
    sealedParents.flatMap(getKey(_)) match {
      case Nil => upickle.core.Annotator.defaultTagKey
      case keys @ (key :: _) if keys.length == sealedParents.length && keys.distinct.length == 1 => key
      case keys =>
        fail(
          s"Type $typeName inherits from multiple parent types with different discriminator keys:\n\n" +
            s"  parents: ${sealedParents.map(getName).sorted.mkString(", ")}\n" +
            s"  keys: ${keys.sorted.mkString(", ")}\n\n" +
            "To resolve this, either remove the `@key` annotations from all parents of the type,\n" +
            "or make sure all the parents pass the same value to `@key`"
        )
    }
}
