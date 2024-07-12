package upickle.core

// Common things for derivation
trait Config {
  /**
   * Specifies the name of the field used to distinguish different `case class`es under
   * a `sealed trait`. Defaults to `$type`, but can be configured globally by overriding
   * [[tagName]], or on a per-`sealed trait` basis via the `@key` annotation
   */
  def tagName = Annotator.defaultTagKey

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


  /**
   * Configure whether you want upickle to skip unknown keys during de-serialization
   * of `case class`es. Can be overriden for the entire serializer via `override def`, and
   * further overriden for individual `case class`es via the annotation
   * `@upickle.implicits.allowUnknownKeys(b: Boolean)`
   */
  def allowUnknownKeys: Boolean = true
}
