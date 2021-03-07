import java.io.ByteArrayOutputStream

import upickle.core.NoOpVisitor

package object upack{
  def transform[T](t: Readable, v: upickle.core.Visitor[_, T]) = t.transform(v)

  /**
    * Read the given MessagePack input into a MessagePack struct
    */
  def read(s: Readable, trace: Boolean = false): Msg = upickle.core.TraceVisitor.withTrace(trace, Msg)(transform(s, _))

  def copy(t: Msg): Msg = transform(t, Msg)
  /**
    * Write the given MessagePack struct as a binary
    */
  def write(t: Msg): Array[Byte] = {
    transform(t, new MsgPackWriter()).toByteArray
  }
  /**
    * Write the given MessagePack struct as a binary to the given OutputStream
    */
  def writeTo(t: Msg, out: java.io.OutputStream): Unit = {
    transform(t, new MsgPackWriter(out))
  }
  def writeToByteArray(t: Msg) = {
    val out = new ByteArrayOutputStream()
    transform(t, new MsgPackWriter(out))
    out.toByteArray
  }
  /**
    * Parse the given MessagePack input, failing if it is invalid
    */
  def validate(s: Readable): Unit = transform(s, NoOpVisitor)
}