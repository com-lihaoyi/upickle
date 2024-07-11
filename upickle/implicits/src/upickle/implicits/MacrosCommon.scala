package upickle.implicits

// Common things for derivation
trait MacrosCommon {
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
   * serialization. Must be kept in sync with [[objectTypeKeyWriteMap]]
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
