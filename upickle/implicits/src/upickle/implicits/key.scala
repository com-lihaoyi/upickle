package upickle.implicits

import scala.annotation.StaticAnnotation

/**
 * Annotation for control over the strings used to serialize your data structure. Can
 * be applied in three ways:
 *
 * 1. To individual fields, in which case it overrides the JSON object fieldname
 *
 * 2. To `case class`es which are part of `sealed trait`s, where it overrides
 *    the value of the `"$type": "foo"` discriminator field
 *
 * 2. To `sealed trait`s themselves, where it overrides
 *    the key of the `"$type": "foo"` discriminator field
 */
class key(s: String) extends StaticAnnotation

/**
 * Annotation for fine-grained control of the `def serializeDefaults` configuration
 * on the upickle bundle; can be applied to individual fields or to entire `case class`es,
 * with finer-grained application taking precedence
 */
class serializeDefaults(s: Boolean) extends StaticAnnotation

/**
 * Annotation for fine-grained control of the `def allowUnknownKeys` configuration
 * on the upickle bundle; can be applied to individual `case class`es, taking precedence
 * over upickle pickler-level configuration
 */
class allowUnknownKeys(b: Boolean) extends StaticAnnotation
