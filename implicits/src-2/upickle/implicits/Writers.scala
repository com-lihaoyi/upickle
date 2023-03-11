package upickle.implicits

import scala.reflect.ClassTag

trait WritersVersionSpecific extends MacroImplicits with WritersVersionSpecificLowPri { this: upickle.core.Types =>

}
trait WritersVersionSpecificLowPri extends WritersVersionSpecificLowLowPri{this: upickle.core.Types =>
}
trait WritersVersionSpecificLowLowPri{this: upickle.core.Types =>

//  implicit def superTypeWriter[T <: Product : ClassTag, V >: T : Writer]: Writer[T] = {
//    implicitly[Writer[V]].comap[T](_.asInstanceOf[V])
//  }
}