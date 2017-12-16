package upickle

import java.io.{ByteArrayOutputStream, PrintStream}

/**
 * Created by haoyi on 8/11/14.
 */
package object json extends JsonPackageWriters{
  def read(s: String): Js.Value = {

    jawn.Parser.parseFromString(s)(json.JawnFacade) match{
      case util.Success(v) => v
      case util.Failure(e) => throw Invalid.Json(e.toString, s)
    }
  }
}
