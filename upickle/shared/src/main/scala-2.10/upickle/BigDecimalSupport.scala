package upickle

import java.math.{ MathContext, BigDecimal => BigDec }
import scala.math.BigDecimal.defaultMathContext

private[upickle] trait BigDecimalSupport {

  /**
   *  In Scala 2.10, [[BigDecimal]] constructors always use the default
   *  [[java.math.MathContext]] so that the input precision is lost sometimes.
   *
   *  [[https://github.com/scala/scala/blob/v2.11.7/src/library/scala/math/BigDecimal.scala#L99-L104]]
   */
  @inline protected def exactBigDecimal(s: String): BigDecimal = {
    val repr = new BigDec(s)
    val mc =
      if (repr.precision <= defaultMathContext.getPrecision) defaultMathContext
      else new MathContext(repr.precision, java.math.RoundingMode.HALF_EVEN)
    BigDecimal(repr, mc)
  }
}
