package upickle

trait Writer[T]{def write: T => Js.Value}
trait Reader[T]{def read: PF[Js.Value, T]}
class WriterCls[T](val write: T => Js.Value) extends Writer[T]
class ReaderCls[T](val read: PF[Js.Value, T]) extends Reader[T]
class ReadWriter[T](val write: T => Js.Value, val read: PF[Js.Value, T]) extends Writer[T] with Reader[T]

