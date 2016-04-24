package upickle

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
  def write(v: Js.Value, indent: Int = 0): String = {
    val sb = new StringBuilder
    FastRenderer.render(sb, 0, v, indent)
    sb.toString
  }
}
