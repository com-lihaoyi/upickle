package upickle.refined

import eu.timepit.refined.api.Refined
import upickle.default.{Reader, Writer}

object RefinedRW {

  implicit def refinedReader[T,C](implicit reader: Reader[T]): Reader[Refined[T, C]] =
    reader.map[Refined[T,C]](Refined.unsafeApply)

  implicit def refinedWriter[T,C](implicit writer: Writer[T]): Writer[Refined[T,C]] = 
    writer.comap(_.value)

}