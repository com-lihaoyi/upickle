package upickle



import scala.reflect.ClassTag
import scala.concurrent.duration.{FiniteDuration, Duration}
import acyclic.file
import scala.language.higherKinds
import scala.language.experimental.macros
import java.util.UUID


/**
* Typeclasses to allow read/writing of all the common
* data-types and data-structures in the standard library
*/
trait Implicits extends Types with Readers with Writers{
}
