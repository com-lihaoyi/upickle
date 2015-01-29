package upickle.config

import upickle.Annotator

/**
 * Represents an object which contains customizations for uPickle behavior.
 *
 * Currently it only contains annotator strategy.
 */
trait Config {
  implicit val annotator: Annotator
}
