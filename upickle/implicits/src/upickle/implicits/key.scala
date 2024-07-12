package upickle.implicits

import scala.annotation.StaticAnnotation

class key(s: String) extends StaticAnnotation
class allowUnknownKeys(b: Boolean) extends StaticAnnotation