package upickle.implicits

// Common things for derivation
trait MacrosCommon {

  def serializeDefaults: Boolean = false

  def objectAttributeKeyReadMap(s: CharSequence): CharSequence = s
  def objectAttributeKeyWriteMap(s: CharSequence): CharSequence = s

  def objectTypeKeyReadMap(s: CharSequence): CharSequence = s
  def objectTypeKeyWriteMap(s: CharSequence): CharSequence = s

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
