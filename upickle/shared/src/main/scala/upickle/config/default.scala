package upickle.config

import upickle.Annotator

trait DefaultConfig extends Config {
  override implicit val annotator = Annotator.keyAnnotator("$variant")
}

object default extends DefaultConfig
