package upickle

import java.io.{ByteArrayOutputStream, PrintStream}
import java.nio.ByteBuffer

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
  def read(s: ByteBuffer): Js.Value = {

    jawn.Parser.parseFromByteBuffer(s)(json.JawnFacade) match{
      case util.Success(v) => v
      case util.Failure(e) => throw e
    }
  }
  def read(s: java.io.File): Js.Value = {

    jawn.Parser.parseFromFile(s)(json.JawnFacade) match{
      case util.Success(v) => v
      case util.Failure(e) => throw e
    }
  }
}
