package upickle

trait Writer[T]{val write: T => Js.Value}
trait Reader[T]{val read: PartialFunction[Js.Value, T]}
class WriterCls[T](val write: T => Js.Value) extends Writer[T]
class ReaderCls[T](val read: PartialFunction[Js.Value, T]) extends Reader[T]
class ReadWriter[T](val write: T => Js.Value, val read: PartialFunction[Js.Value, T]) extends Writer[T] with Reader[T]

