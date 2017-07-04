package upickle

private[upickle] trait BigDecimalSupport {

  @inline protected def exactBigDecimal(s: String): BigDecimal = BigDecimal(s)
}
