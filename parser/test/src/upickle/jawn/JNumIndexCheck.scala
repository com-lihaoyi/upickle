package upickle
package jawn
package parser

import java.nio.ByteBuffer
import org.scalatest.{Matchers, PropSpec}
import org.scalatest.prop.PropertyChecks
import scala.util.Success
object JNumIndexCheckFacade extends Visitor[Boolean, Boolean] {
  def singleContext(index: Int) = new ObjVisitor[Boolean, Boolean] {
    var failed = false
    def subVisitor = JNumIndexCheckFacade
    def visitKey(s: CharSequence, index: Int): Unit = ()
    def add(v: Boolean, index: Int): Unit = {
      if (!v) failed = true
    }
    def finish(index: Int): Boolean = !failed
  }
  def arrayContext(index: Int)  = new ArrVisitor[Boolean, Boolean] {
    var failed = false
    def subVisitor = JNumIndexCheckFacade
    def visitKey(s: CharSequence, index: Int): Unit = ()
    def add(v: Boolean, index: Int): Unit = {
      if (!v) failed = true
    }
    def finish(index: Int): Boolean = !failed
  }
  def objectContext(index: Int) = new ObjVisitor[Boolean, Boolean] {
    var failed = false
    def subVisitor = JNumIndexCheckFacade
    def visitKey(s: CharSequence, index: Int): Unit = ()
    def add(v: Boolean, index: Int): Unit = {
      if (!v) failed = true
    }
    def finish(index: Int): Boolean = !failed
  }

  def jnull(index: Int): Boolean = true
  def jfalse(index: Int): Boolean = true
  def jtrue(index: Int): Boolean = true
  def jnum(s: CharSequence, decIndex: Int, expIndex: Int, index: Int): Boolean = {
    val input = s.toString
    val inputDecIndex = input.indexOf('.')
    val inputExpIndex = if (input.indexOf('e') == -1) input.indexOf("E") else input.indexOf('e')

    decIndex == inputDecIndex && expIndex == inputExpIndex
  }
  def jstring(s: CharSequence, index: Int): Boolean = true
}


class JNumIndexCheck extends PropSpec with Matchers with PropertyChecks {

  property("jnum provides the correct indices with parseFromString") {
    forAll { (value: BigDecimal) =>
      val json = s"""{ "num": ${value.toString} }"""
      Parser.parseFromString(json)(JNumIndexCheckFacade) shouldBe Success(true)
    }
  }

  property("jnum provides the correct indices with parseFromByteBuffer") {
    forAll { (value: BigDecimal) =>
      val json = s"""{ "num": ${value.toString} }"""
      val bb = ByteBuffer.wrap(json.getBytes("UTF-8"))
      Parser.parseFromByteBuffer(bb)(JNumIndexCheckFacade) shouldBe Success(true)
    }
  }

  property("jnum provides the correct indices at the top level with parseFromString") {
    forAll { (value: BigDecimal) =>
      Parser.parseFromString(value.toString)(JNumIndexCheckFacade) shouldBe Success(true)
    }
  }

  property("jnum provides the correct indices at the top level with parseFromByteBuffer") {
    forAll { (value: BigDecimal) =>
      val bb = ByteBuffer.wrap(value.toString.getBytes("UTF-8"))
      Parser.parseFromByteBuffer(bb)(JNumIndexCheckFacade) shouldBe Success(true)
    }
  }

}
