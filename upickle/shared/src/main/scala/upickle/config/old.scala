package upickle.config

import upickle.Annotator

trait OldConfig extends Config {
  override implicit val annotator = Annotator.arrayAnnotator
}

object old extends OldConfig
