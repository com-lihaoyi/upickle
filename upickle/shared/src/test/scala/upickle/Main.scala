package upickle


object Main extends App {
    import Generic.ADT
    import Hierarchy._
    import Recursive._
    import Defaults._
    import ADTs.ADT0
  type Data = ADT[Seq[(Int, Int)], String, A, LL, ADTc, ADT0]

//  old.write[Data](???)
//  old.write[ADTs.ADTb](???)
//  old.write[Varargs.Sentence](???)
//  old.write[MixedIn.Obj.ClsB](???)

}
