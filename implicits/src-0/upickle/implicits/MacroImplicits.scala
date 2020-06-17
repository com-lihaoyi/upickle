package upickle.implicits

// This trait is needed only for
// JsReadWriters and MsgReadWriters
// to take priority over macro implicits.
// TODO come up with a better design without
// this dummy trait
trait MacroImplicits
