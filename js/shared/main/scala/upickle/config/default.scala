package upickle.config

import upickle.Annotator

trait Default extends Config {
  override implicit val annotator = Annotator.keyAnnotator("$variant")
}

object default extends Default
