package test.pprint
import utest._
object Check{
  def apply[T: pprint.PPrint](t: T, expected: String*)(implicit cfg: pprint.Config) = {
    val pprinted = pprint.tokenize(t)(implicitly, cfg)
                         .mkString
                         .replaceAll("\u001B\\[[;\\d]*m", "")
    assert(expected.map(_.trim).contains(pprinted))
  }


}