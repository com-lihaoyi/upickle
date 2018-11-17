package upickle

import scala.scalajs.js

trait WebJson extends upickle.core.Types {
  object web {
    def read[T: Reader](s: String) = {
      ujson.WebJson.transform(js.JSON.parse(s), implicitly[Reader[T]])
    }

    def write[T: Writer](t: T, indent: Int = -1) = {
      js.JSON.stringify(implicitly[Writer[T]].write(ujson.WebJson.Builder, t))
    }
  }
}
