package upickle.implicits

// Common things for derivation
trait MacrosCommon {

  def serializeDefaults: Boolean = false

  def objectAttributeKeyReadMap(s: CharSequence): CharSequence = s
  def objectAttributeKeyWriteMap(s: CharSequence): CharSequence = s

  def objectTypeKeyReadMap(s: CharSequence): CharSequence = s
  def objectTypeKeyWriteMap(s: CharSequence): CharSequence = s

}

