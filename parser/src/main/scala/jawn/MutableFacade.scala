package jawn

import scala.collection.mutable

trait MutableFacade[J] extends Facade[J] {
  def jarray(vs: mutable.ArrayBuffer[J]): J
  def jobject(vs: mutable.Map[String, J]): J

  def singleContext() = new FContext[J] {
    def facade = MutableFacade.this
    var value: J = _
    def visitKey(s: CharSequence) { value = jstring(s) }
    def add(v: J) { value = v }
    def finish: J = value
    def isObj: Boolean = false
  }

  def arrayContext() = new FContext[J] {
    def facade = MutableFacade.this
    val vs = mutable.ArrayBuffer.empty[J]
    def visitKey(s: CharSequence) { vs.append(jstring(s)) }
    def add(v: J) { vs.append(v) }
    def finish: J = jarray(vs)
    def isObj: Boolean = false
  }

  def objectContext() = new FContext[J] {
    def facade = MutableFacade.this
    var key: String = null
    val vs = mutable.Map.empty[String, J]
    def visitKey(s: CharSequence): Unit =
      if (key == null) { key = s.toString } else { vs(key) = jstring(s); key = null }
    def add(v: J): Unit =
      { vs(key) = v; key = null }
    def finish = jobject(vs)
    def isObj = true
  }
}
