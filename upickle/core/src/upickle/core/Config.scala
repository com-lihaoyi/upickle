package upickle.core

// Common things for derivation
trait Config {
  def tagName = Annotator.defaultTagKey

  /**
   * Whether to use the fully-qualified name of `case class`es and `case object`s which
   * are part of `sealed trait` hierarchies when serializing them and writing their `$type`
   * key. Defaults to `false`, so `$type` key uses the shortest partially-qualified name.
   * Can be set to `true` to use their fully-qualified name.
   * */
  def objectTypeKeyWriteFullyQualified: Boolean = false

  /**
   * Whether or not `case class` fields with values equal to the default are serialized.
   * Defaults to `false`, so such fields are elided.
   */
  def serializeDefaults: Boolean = false

  /**
   * Map the name of Scala `case class` fields to JSON object fields during de-serialization.
   * Must be kept in sync with [[objectAttributeKeyWriteMap]]
   */
  def objectAttributeKeyReadMap(s: CharSequence): CharSequence = s

  /**
   * Map the name of JSON object fields to Scala `case class` fields during serialization.
   * Must be kept in sync with [[objectAttributeKeyReadMap]]
   */
  def objectAttributeKeyWriteMap(s: CharSequence): CharSequence = s

  /**
   * Map the name of JSON `$type` field values to Scala `case class` type names during
   * de-serialization. Must be kept in sync with [[objectTypeKeyWriteMap]]
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
