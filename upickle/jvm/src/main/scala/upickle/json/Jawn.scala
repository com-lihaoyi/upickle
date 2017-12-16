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
  def jstring(s: String) = Js.Str(s)
  def jarray(vs: mutable.ArrayBuffer[Js.Value]) = Js.Arr(vs:_*)
  def jobject(vs: mutable.ArrayBuffer[(String, Js.Value)]) = Js.Obj(vs:_*)
}

private[json] trait MutableFacade[J] extends Facade[J] {
  def jarray(vs: mutable.ArrayBuffer[J]): J
  def jobject(vs: mutable.ArrayBuffer[(String, J)]): J

  def singleContext() = new FContext[J] {
    var value: J = _
    def add(s: String) { value = jstring(s) }
    def add(v: J) { value = v }
    def finish: J = value
    def isObj: Boolean = false
  }

  def arrayContext() = new FContext[J] {
    val vs = mutable.ArrayBuffer.empty[J]
    def add(s: String) { vs.append(jstring(s)) }
    def add(v: J) { vs.append(v) }
    def finish: J = jarray(vs)
    def isObj: Boolean = false
  }

  def objectContext() = new FContext[J] {
    var key: String = null
    val vs = mutable.ArrayBuffer.empty[(String, J)]
    def add(s: String): Unit =
      if (key == null) { key = s } else { vs += (key -> jstring(s)); key = null }
    def add(v: J): Unit =
    { vs += (key -> v); key = null }
    def finish = jobject(vs)
    def isObj = true
  }
}
