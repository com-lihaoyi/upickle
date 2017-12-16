import upickle.default
import upickle.default.{macroRW, ReadWriter => RW, Reader => R, Writer => W}


object Main{
  case class SingleNode(value: Int, children: List[SingleTree]) extends SingleTree

  object SingleNode{
    implicit def SingleNoderw: R[SingleNode] = upickle.legacy.macroRW[SingleNode]
  }
  sealed trait SingleTree

  object SingleTree{
    implicit def SingleTreerw: R[SingleTree] = upickle.legacy.macroRW[SingleTree]
  }

  def main(args: Array[String]): Unit = {
  }
}
