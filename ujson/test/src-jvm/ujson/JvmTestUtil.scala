package ujson

import java.io._

object JvmTestUtil {
  def withTemp[A](s: String)(f: File => A): A = {
    val t = File.createTempFile("jawn-syntax", ".json")
    val pw = new PrintWriter(t)
    pw.println(s)
    pw.close()
    try {
      f(t)
    } finally {
      t.delete()
    }
  }
}
