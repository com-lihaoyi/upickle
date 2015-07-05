package upickle

import upickle.Recursive.IntTree


object Main extends App {
  implicitly[upickle.old.Reader[MixedIn.Obj.ClsB]]
  TestUtil.rw(
    IntTree(123, List(IntTree(456, Nil), IntTree(789, Nil))),
    """{"value":123,"children":[{"value":456,"children":[]},{"value":789,"children":[]}]}"""
  )
}