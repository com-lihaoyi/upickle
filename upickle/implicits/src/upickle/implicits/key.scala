package upickle.implicits

import scala.annotation.StaticAnnotation

case class key(s: String) extends StaticAnnotation
case class allowUnknownKeys(b: Boolean) extends StaticAnnotation