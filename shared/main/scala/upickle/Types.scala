package upickle

trait Writer[T]{val write: T => Js.Value}
trait Reader[T]{val read: Js.Value => T}
class WriterCls[T](val write: T => Js.Value) extends Writer[T]
class ReaderCls[T](val read: Js.Value => T) extends Reader[T]
class ReadWriter[T](val write: T => Js.Value, val read: Js.Value => T) extends Writer[T] with Reader[T]

