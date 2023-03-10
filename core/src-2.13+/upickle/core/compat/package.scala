package upickle.core

package object compat {

  type Factory[-A, +C] = collection.Factory[A, C]

}
