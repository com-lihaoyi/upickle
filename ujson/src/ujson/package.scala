import upickle.core.NoOpVisitor
import upickle.core.BufferedValue

package object ujson{
  def transform[T](t: Readable,
                   v: upickle.core.Visitor[_, T],
                   sortKeys: Boolean = false): T = {
    BufferedValue.maybeSortKeysTransform(Readable, t, sortKeys, v)
  }

  def transform[T](t: Readable,
                   v: upickle.core.Visitor[_, T]): T = transform(t, v, sortKeys = false)
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
            sortKeys: Boolean = false): String = {
    val writer = new java.io.StringWriter
    writeTo(t, writer, indent, escapeUnicode, sortKeys)
    writer.toString
  }
  def write(t: Value.Value,
            indent: Int,
            escapeUnicode: Boolean): String = {
    write(t, indent, escapeUnicode, sortKeys = false)
  }

  /**
    * Write the given JSON struct as a JSON String to the given Writer
    */
  def writeTo(t: Value.Value,
              out: java.io.Writer,
              indent: Int = -1,
              escapeUnicode: Boolean = false,
              sortKeys: Boolean = false): Unit = {
    transform(t, Renderer(out, indent, escapeUnicode), sortKeys)
  }
  def writeTo(t: Value.Value,
              out: java.io.Writer,
              indent: Int,
              escapeUnicode: Boolean): Unit = {
    writeTo(t, out, indent, escapeUnicode, sortKeys=false)
  }

  def writeToOutputStream(t: Value.Value,
                          out: java.io.OutputStream,
                          indent: Int = -1,
                          escapeUnicode: Boolean = false,
                          sortKeys: Boolean = false): Unit = {
    transform(t, new BaseByteRenderer(out, indent, escapeUnicode), sortKeys)
  }

  def writeToOutputStream(t: Value.Value,
                          out: java.io.OutputStream,
                          indent: Int,
                          escapeUnicode: Boolean): Unit = {
    writeToOutputStream(t, out, indent, escapeUnicode, sortKeys = false)
  }

  def writeToByteArray(t: Value.Value,
                       indent: Int = -1,
                       escapeUnicode: Boolean = false,
                       sortKeys: Boolean = false): Array[Byte] = {
    val baos = new java.io.ByteArrayOutputStream
    writeToOutputStream(t, baos, indent, escapeUnicode, sortKeys)
    baos.toByteArray
  }

  def writeToByteArray(t: Value.Value,
                       indent: Int,
                       escapeUnicode: Boolean): Array[Byte] = {
    writeToByteArray(t, indent, escapeUnicode, sortKeys = false)
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
               sortKeys: Boolean = false): String = {
    val writer = new java.io.StringWriter()
    reformatTo(s, writer, indent, escapeUnicode, sortKeys)
    writer.toString
  }
  def reformat(s: Readable,
               indent: Int,
               escapeUnicode: Boolean): String = {
    reformat(s, indent, escapeUnicode, sortKeys = false)
  }
  /**
    * Parse the given JSON input and write it to a string with
    * the configured formatting to the given Writer
    */
  def reformatTo(s: Readable,
                 out: java.io.Writer,
                 indent: Int = -1,
                 escapeUnicode: Boolean = false,
                 sortKeys: Boolean = false): Unit = {
    transform(s, Renderer(out, indent, escapeUnicode), sortKeys)
  }
  def reformatTo(s: Readable,
                 out: java.io.Writer,
                 indent: Int,
                 escapeUnicode: Boolean): Unit = {
    reformatTo(s, out, indent, escapeUnicode, sortKeys = false)
  }
  /**
    * Parse the given JSON input and write it to a string with
    * the configured formatting to the given Writer
    */
  def reformatToOutputStream(s: Readable,
                             out: java.io.OutputStream,
                             indent: Int = -1,
                             escapeUnicode: Boolean = false,
                             sortKeys: Boolean = false): Unit = {
    transform(s, new BaseByteRenderer(out, indent, escapeUnicode), sortKeys)
  }
  def reformatToOutputStream(s: Readable,
                             out: java.io.OutputStream,
                             indent: Int,
                             escapeUnicode: Boolean): Unit = {
    reformatToOutputStream(s, out, indent, escapeUnicode, sortKeys = false)
  }

  def reformatToByteArray(s: Readable,
                          indent: Int = -1,
                          escapeUnicode: Boolean = false,
                          sortKeys: Boolean = false): Array[Byte] = {
    val baos = new java.io.ByteArrayOutputStream
    reformatToOutputStream(s, baos, indent, escapeUnicode, sortKeys)
    baos.toByteArray
  }

  def reformatToByteArray(s: Readable,
                          indent: Int,
                          escapeUnicode: Boolean): Array[Byte] = {
    reformatToByteArray(s, indent, escapeUnicode, sortKeys = false)
  }
  // End ujson
  @deprecated("use ujson.Value")
  type Js = Value
  @deprecated("use ujson.Value")
  val Js = Value
}
