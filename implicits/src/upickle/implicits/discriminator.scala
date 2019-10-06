package upickle.implicits

import scala.annotation.StaticAnnotation

/**
  * Defines the name of a new field (not defined in the class itself)
  * to serve as the discriminator property name for identifying subclasses.
  *
  * For naming, consider that some implementations (e.g. vpack) may sort object keys,
  * so symbol prefixes work well for ensuring the tag is the first property.
  * Readers will fast path if this is the first field of the object.
  * Otherwise, Readers will have to buffer the content and find the tag later.
  *
  * You can also tag the subclasses with [[key]] to override values for this field,
  * which will otherwise default to the FQCN of the classname.
  *
  * @see https://swagger.io/docs/specification/data-models/inheritance-and-polymorphism/
  */
case class discriminator(propertyName: String = "$type") extends StaticAnnotation
