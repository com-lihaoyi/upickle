package ujson

import java.io.ByteArrayOutputStream

import upickle.core.{ArrVisitor, ObjVisitor, Visitor}

import scala.annotation.switch

case class BytesRenderer(indent: Int = -1, escapeUnicode: Boolean = false)
  extends BaseByteRenderer(new ByteArrayOutputStream(), indent, escapeUnicode){
}


case class StringRenderer(indent: Int = -1,
                          escapeUnicode: Boolean = false)
  extends BaseCharRenderer(new java.io.StringWriter(), indent, escapeUnicode)

case class Renderer(out: java.io.Writer,
                    indent: Int = -1,
                    escapeUnicode: Boolean = false)
  extends BaseCharRenderer(out, indent, escapeUnicode)
