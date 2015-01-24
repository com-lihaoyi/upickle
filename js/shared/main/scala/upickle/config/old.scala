package upickle.config

import upickle.Annotator

trait Old extends Config {
  override implicit val annotator = Annotator.arrayAnnotator
}

object old extends Old
