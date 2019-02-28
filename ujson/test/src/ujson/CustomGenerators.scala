package ujson

import org.scalatest.prop.Generator

object CustomGenerators {
  implicit val genBigDecimal: Generator[BigDecimal] =
      for {
        n <- Generator.longGenerator
        d <- Generator.longGenerator.filter(_ != 0D)
      } yield BigDecimal(n) / d.toDouble
}