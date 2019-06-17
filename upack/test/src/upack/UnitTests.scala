package upack
import upickle.core.Abort
import utest._

object UnitTests extends TestSuite{
  val tests = Tests {
    test("compositeKeys"){
      val msg = Obj(Arr(Int32(1), Int32(2)) -> Int32(1))
      val written = upack.write(msg)
      val writtenStr = upickle.core.Util.bytesToString(written)
      writtenStr ==> "81-92-01-02-01"

      upack.read(written) ==> msg


      intercept[Abort]{
        upack.transform(written, ujson.Value)
      }
      intercept[Abort] {
        upack.transform(msg, ujson.Value)
      }
    }
  }
}