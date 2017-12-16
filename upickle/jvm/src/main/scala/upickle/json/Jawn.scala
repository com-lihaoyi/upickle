package upickle
package json
import java.io.{ByteArrayInputStream, ByteArrayOutputStream, PrintStream}

import jawn._

import collection.mutable
import acyclic.file

import scala.annotation.switch

private[json] object JawnFacade extends MutableFacade[Js.Value] {
  def jnull() = Js.Null
  def jfalse() = Js.False
  def jtrue() = Js.True
  def jnum(s: String) = Js.Num(s.toDouble)
  def jint(s: String) = Js.Num(s.toDouble)
  def jarray(vs: mutable.ArrayBuffer[Js.Value]) = Js.Arr(vs:_*)
  def jobject(vs: mutable.ArrayBuffer[(CharSequence, Js.Value)]) = Js.Obj(vs:_*)

  def jnum(s: CharSequence, decIndex: Int, expIndex: Int) = Js.Num(s.toString.toDouble)


  def jstring(s: CharSequence) = Js.Str(s.toString)

}

private[json] trait MutableFacade[J] extends Facade[J] {
  def jarray(vs: mutable.ArrayBuffer[J]): J
  def jobject(vs: mutable.ArrayBuffer[(CharSequence, J)]): J

  def singleContext() = new FContext[J] {
    var value: J = _
    def add(v: J) { value = v }
    def finish: J = value
    def isObj: Boolean = false

    def add(s: CharSequence): Unit = { value = jstring(s) }
  }

  def arrayContext() = new FContext[J] {
    val vs = mutable.ArrayBuffer.empty[J]
    def add(v: J) { vs.append(v) }
    def finish: J = jarray(vs)
    def isObj: Boolean = false

    def add(s: CharSequence): Unit = vs.append(jstring(s))
  }

  def objectContext() = new FContext[J] {
    var key: CharSequence = null
    val vs = mutable.ArrayBuffer.empty[(CharSequence, J)]

    def add(v: J): Unit =
    { vs += (key -> v); key = null }
    def finish = jobject(vs)
    def isObj = true

    def add(s: CharSequence): Unit = {
      if (key == null) { key = s } else { vs += (key -> jstring(s)); key = null }
    }
  }
}
