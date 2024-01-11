package upickle.implicits

import scala.annotation.StaticAnnotation

case class key(s: String) extends StaticAnnotation
case class ignoreUnknownKeys(b: Boolean) extends StaticAnnotation