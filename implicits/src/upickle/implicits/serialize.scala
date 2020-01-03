package upickle.implicits

import scala.annotation.StaticAnnotation

case class serialize(defaultValues: Boolean) extends StaticAnnotation