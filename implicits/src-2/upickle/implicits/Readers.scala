package upickle.implicits

trait ReadersVersionSpecific extends MacroImplicits with LowPriReadersVersionSpecific { this: upickle.core.Types =>

}

trait LowPriReadersVersionSpecific extends LowLowPriReadersVersionSpecific {this: upickle.core.Types =>

}

trait LowLowPriReadersVersionSpecific{this: upickle.core.Types =>

//  implicit def superTypeReader[T <: Product, V >: T : Reader]: Reader[T] = {
//    val actual = implicitly[Reader[V]].asInstanceOf[TaggedReader[T]]
    //    val tagName = macros.tagName[T]
    //    new TaggedReader.Leaf(tagName, actual.findReader(tagName))
//    actual //.findReader(tagName)
//  }

}
