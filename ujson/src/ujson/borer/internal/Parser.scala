package ujson.borer.internal

import ujson.borer.{Input, Receiver, ByteAccess}


/**
  * Common parent type of [[io.bullet.borer.cbor.CborParser]] and [[io.bullet.borer.json.JsonParser]]
  */
abstract private[borer] class Parser[Bytes] extends Input.PaddingProvider[Bytes] {

  /**
    * The [[Input]] the parser is parsing from.
    */
  def input: Input[Bytes]

  /**
    * The index of the first byte of the value that was produced by the last call to `pull`.
    */
  def valueIndex: Long

  /**
    * Sends the next data item to the given [[Receiver]].
    * The given [[Receiver]] receives exactly one call to one of its methods.
    * The returned `Int` is the [[io.bullet.borer.DataItem]] code for the value the [[Receiver]] received.
    */
  def pull(receiver: Receiver): Int
}

private[borer] object Parser {

  type Creator[Bytes, Config] = (Input[Bytes], ByteAccess[Bytes], Config) => Parser[Bytes]

  type Wrapper[Config] = (Receiver, Config) => Receiver
  private[this] val _nopWrapper: Wrapper[Any] = (receiver, _) => receiver

  def nopWrapper[Config]: Wrapper[Config] = _nopWrapper.asInstanceOf[Wrapper[Config]]
}