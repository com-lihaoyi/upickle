package upickle
package jawn
package parser

import java.nio.ByteBuffer
import org.scalatest.{Matchers, PropSpec}
import org.scalatest.prop.PropertyChecks
import scala.util.Success
object JNumIndexCheckFacade extends Visitor[Boolean, Boolean] {
  def visitArray(index: Int)  = new ArrVisitor[Boolean, Boolean] {
    var failed = false
    def subVisitor = JNumIndexCheckFacade
    def visitKey(s: CharSequence, index: Int): Unit = ()
    def visitValue(v: Boolean, index: Int): Unit = {
      if (!v) failed = true
    }
    def visitEnd(index: Int): Boolean = !failed
  }
  def visitObject(index: Int) = new ObjVisitor[Boolean, Boolean] {
    var failed = false
    def subVisitor = JNumIndexCheckFacade
    def visitKey(s: CharSequence, index: Int): Unit = ()
    def visitValue(v: Boolean, index: Int): Unit = {
      if (!v) failed = true
    }
    def visitEnd(index: Int): Boolean = !failed
  }

  def visitNull(index: Int): Boolean = true
  def visitFalse(index: Int): Boolean = true
  def visitTrue(index: Int): Boolean = true
  def visitNum(s: CharSequence, decIndex: Int, expIndex: Int, index: Int): Boolean = {
    val input = s.toString
    val inputDecIndex = input.indexOf('.')
    val inputExpIndex = if (input.indexOf('e') == -1) input.indexOf("E") else input.indexOf('e')

    decIndex == inputDecIndex && expIndex == inputExpIndex
  }
  def visitString(s: CharSequence, index: Int): Boolean = true
}


class JNumIndexCheck extends PropSpec with Matchers with PropertyChecks {

  property("visitNum provides the correct indices with parseFromString") {
    forAll { (value: BigDecimal) =>
      val json = s"""{ "num": ${value.toString} }"""
      StringParser.walk(json, JNumIndexCheckFacade) shouldBe true
    }
  }

  property("visitNum provides the correct indices with parseFromByteBuffer") {
    forAll { (value: BigDecimal) =>
      val json = s"""{ "num": ${value.toString} }"""
      val bb = ByteBuffer.wrap(json.getBytes("UTF-8"))
      ByteBufferParser.walk(bb, JNumIndexCheckFacade) shouldBe true
    }
  }

  property("visitNum provides the correct indices at the top level with parseFromString") {
    forAll { (value: BigDecimal) =>
      StringParser.walk(value.toString, JNumIndexCheckFacade) shouldBe true
    }
  }

  property("visitNum provides the correct indices at the top level with parseFromByteBuffer") {
    forAll { (value: BigDecimal) =>
      val bb = ByteBuffer.wrap(value.toString.getBytes("UTF-8"))
      ByteBufferParser.walk(bb, JNumIndexCheckFacade) shouldBe true
    }
  }
}
