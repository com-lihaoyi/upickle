package upickle
import acyclic.file
/**
 * Created by haoyi on 8/11/14.
 */
package object json {
  def read(s: String): Js.Value = {

    jawn.Parser.parseFromString(s)(json.JawnFacade) match{
      case util.Success(v) => v
      case util.Failure(e) => throw Invalid.Json(e.toString, s)
    }
  }

  def readOption(s: String): Option[Js.Value] = {
    try {
      Some(read(s))
    } catch {
      case e: Throwable => None
    }
  }

  def write(v: Js.Value): String = {
    FastRenderer.render(v)
  }
}
