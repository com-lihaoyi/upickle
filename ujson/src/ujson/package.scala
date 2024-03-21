import upickle.core.NoOpVisitor
import upickle.core.BufferedValue
import scala.annotation.unroll
package object ujson{
  def transform[T](t: Readable,
                   v: upickle.core.Visitor[_, T],
                   @unroll sortKeys: Boolean = false): T = {
    BufferedValue.maybeSortKeysTransform(Readable, t, sortKeys, v)
  }

  /**
    * Read the given JSON input as a JSON struct
    */
  def read(s: Readable, trace: Boolean = false): Value.Value =
    upickle.core.TraceVisitor.withTrace(trace, Value)(transform(s, _))

  def copy(t: Value.Value): Value.Value = transform(t, Value)

  /**
    * Write the given JSON struct as a JSON String
    */
  def write(t: Value.Value,
            indent: Int = -1,
            escapeUnicode: Boolean = false,
            @unroll sortKeys: Boolean = false): String = {
    val writer = new java.io.StringWriter
    writeTo(t, writer, indent, escapeUnicode, sortKeys)
    writer.toString
  }

  /**
    * Write the given JSON struct as a JSON String to the given Writer
    */
  def writeTo(t: Value.Value,
              out: java.io.Writer,
              indent: Int = -1,
              escapeUnicode: Boolean = false,
              @unroll sortKeys: Boolean = false): Unit = {
    transform(t, Renderer(out, indent, escapeUnicode), sortKeys)
  }

  def writeToOutputStream(t: Value.Value,
                          out: java.io.OutputStream,
                          indent: Int = -1,
                          escapeUnicode: Boolean = false,
                          @unroll sortKeys: Boolean = false): Unit = {
    transform(t, new BaseByteRenderer(out, indent, escapeUnicode), sortKeys)
  }

  def writeToByteArray(t: Value.Value,
                       indent: Int = -1,
                       escapeUnicode: Boolean = false,
                       @unroll sortKeys: Boolean = false): Array[Byte] = {
    val baos = new java.io.ByteArrayOutputStream
    writeToOutputStream(t, baos, indent, escapeUnicode, sortKeys)
    baos.toByteArray
  }

  /**
    * Parse the given JSON input, failing if it is invalid
    */
  def validate(s: Readable): Unit = transform(s, NoOpVisitor)
  /**
    * Parse the given JSON input and write it to a string with
    * the configured formatting
    */
  def reformat(s: Readable,
               indent: Int = -1,
               escapeUnicode: Boolean = false,
               @unroll sortKeys: Boolean = false): String = {
    val writer = new java.io.StringWriter()
    reformatTo(s, writer, indent, escapeUnicode, sortKeys)
    writer.toString
  }

  /**
    * Parse the given JSON input and write it to a string with
    * the configured formatting to the given Writer
    */
  def reformatTo(s: Readable,
                 out: java.io.Writer,
                 indent: Int = -1,
                 escapeUnicode: Boolean = false,
                 @unroll sortKeys: Boolean = false): Unit = {
    transform(s, Renderer(out, indent, escapeUnicode), sortKeys)
  }

  /**
    * Parse the given JSON input and write it to a string with
    * the configured formatting to the given Writer
    */
  def reformatToOutputStream(s: Readable,
                             out: java.io.OutputStream,
                             indent: Int = -1,
                             escapeUnicode: Boolean = false,
                             @unroll sortKeys: Boolean = false): Unit = {
    transform(s, new BaseByteRenderer(out, indent, escapeUnicode), sortKeys)
  }

  def reformatToByteArray(s: Readable,
                          indent: Int = -1,
                          escapeUnicode: Boolean = false,
                          @unroll sortKeys: Boolean = false): Array[Byte] = {
    val baos = new java.io.ByteArrayOutputStream
    reformatToOutputStream(s, baos, indent, escapeUnicode, sortKeys)
    baos.toByteArray
  }

  // End ujson
  @deprecated("use ujson.Value")
  type Js = Value
  @deprecated("use ujson.Value")
  val Js = Value
}
