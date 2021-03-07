import upickle.core.NoOpVisitor

package object ujson{
  def transform[T](t: Readable, v: upickle.core.Visitor[_, T]) = t.transform(v)

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
            escapeUnicode: Boolean = false): String = {
    val writer = new java.io.StringWriter
    writeTo(t, writer, indent, escapeUnicode)
    writer.toString
  }

  /**
    * Write the given JSON struct as a JSON String to the given Writer
    */
  def writeTo(t: Value.Value,
              out: java.io.Writer,
              indent: Int = -1,
              escapeUnicode: Boolean = false): Unit = {
    transform(t, Renderer(out, indent, escapeUnicode))
  }
  def writeToOutputStream(t: Value.Value,
                          out: java.io.OutputStream,
                          indent: Int = -1,
                          escapeUnicode: Boolean = false): Unit = {
    transform(t, new BaseByteRenderer(out, indent, escapeUnicode))
  }

  def writeToByteArray(t: Value.Value,
                       indent: Int = -1,
                       escapeUnicode: Boolean = false) = {
    val baos = new java.io.ByteArrayOutputStream
    writeToOutputStream(t, baos, indent, escapeUnicode)
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
  def reformat(s: Readable, indent: Int = -1, escapeUnicode: Boolean = false): String = {
    val writer = new java.io.StringWriter()
    reformatTo(s, writer, indent, escapeUnicode)
    writer.toString
  }
  /**
    * Parse the given JSON input and write it to a string with
    * the configured formatting to the given Writer
    */
  def reformatTo(s: Readable, out: java.io.Writer, indent: Int = -1, escapeUnicode: Boolean = false): Unit = {
    transform(s, Renderer(out, indent, escapeUnicode))
  }
  /**
    * Parse the given JSON input and write it to a string with
    * the configured formatting to the given Writer
    */
  def reformatToOutputStream(s: Readable,
                             out: java.io.OutputStream,
                             indent: Int = -1,
                             escapeUnicode: Boolean = false): Unit = {
    transform(s, new BaseByteRenderer(out, indent, escapeUnicode))
  }
  def reformatToByteArray(s: Readable,
                          indent: Int = -1,
                          escapeUnicode: Boolean = false) = {
    val baos = new java.io.ByteArrayOutputStream
    reformatToOutputStream(s, baos, indent, escapeUnicode)
    baos.toByteArray
  }
  // End ujson
  @deprecated("use ujson.Value")
  type Js = Value
  @deprecated("use ujson.Value")
  val Js = Value
}
