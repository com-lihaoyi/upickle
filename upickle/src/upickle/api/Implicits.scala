package upickle
package api

/**
* Typeclasses to allow read/writing of all the common
* data-types and data-structures in the standard library
*/
trait Implicits extends upickle.core.Types with Readers with Writers
