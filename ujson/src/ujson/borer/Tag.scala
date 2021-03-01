package ujson.borer


import ujson.borer.internal.Util

/**
  * Abstraction for a "tag" in CBOR-speak.
  *
  * @param code the tag's code
  */
sealed abstract class Tag(final val code: Long) {

  def @@[T](obj: T): TaggedValue[T] = TaggedValue(this, obj)
}

object Tag {

  /**
    * The element following this tag is a date/time string that follows the standard format
    * described in [RFC3339], as refined by Section 3.3 of [RFC4287].
    *
    * @see https://tools.ietf.org/html/rfc4287#section-3.3
    */
  final case object DateTimeString extends Tag(0)

  /**
    * The element following this tag is a numerical representation of seconds relative to
    * 1970-01-01T00:00Z in UTC time.  (For the non-negative values that the
    * Portable Operating System Interface (POSIX) defines, the number of
    * seconds is counted in the same way as for POSIX "seconds since the
    * epoch" [TIME_T].)  The tagged item can be an Int, Long, Float or Double.
    * Note that the number can be negative (time before 1970-01-01T00:00Z) and,
    * if a floating-point number, indicate fractional seconds.
    */
  final case object EpochDateTime extends Tag(1)

  /**
    * The element following this tag is a byte string data item,
    * which is interpreted as an unsigned integer n in network byte order.
    * There might be leading zeroes!
    *
    * NOTE: This tag isn't actually ever produced by the CBOR parser,
    * since it has built-in support for it and immediately produces a
    * [[java.math.BigInteger]] instead.
    */
  final case object PositiveBigNum extends Tag(2)

  /**
    * The element following this tag is a byte string data item,
    * which is interpreted as an unsigned integer n in network byte order.
    * The semantic value is -1 - n.
    * There might be leading zeroes!
    *
    * NOTE: This tag isn't actually ever produced by the CBOR parser,
    * since it has built-in support for it and immediately produces a
    * [[java.math.BigInteger]] instead.
    */
  final case object NegativeBigNum extends Tag(3)

  /**
    * The element following this tag is an array
    * that contains exactly two integer numbers:
    * 1. Exponent e with base 10 (must be Int or Long, i.e. BigNum is disallowed)
    * 2. Mantissa m (Int, Long or BigNum)
    * The value of the decimal fraction is m*(10**e).
    *
    * NOTE: This tag isn't actually ever produced by the CBOR parser,
    * since it has built-in support for it and immediately produces a
    * [[java.math.BigDecimal]] instead.
    */
  final case object DecimalFraction extends Tag(4)

  /**
    * The element following this tag is an array
    * that contains exactly two integer numbers:
    * 1. Exponent e with base 2 (must be Int or Long, i.e. BigNum is disallowed)
    * 2. Mantissa m (Int, Long or BigNum)
    * The value of the bigfloat is is m*(2**e).
    */
  final case object BigFloat extends Tag(5)

  /**
    * The element following this tag is a byte string,
    * which should be base64url-encoded in potential downstream
    * text-based representation like JSON.
    */
  final case object HintBase64url extends Tag(21)

  /**
    * The element following this tag is a byte string,
    * which should be base64-encoded in potential downstream
    * text-based representation like JSON.
    */
  final case object HintBase64 extends Tag(22)

  /**
    * The element following this tag is a byte string,
    * which should be base16-encoded in potential downstream
    * text-based representation like JSON.
    */
  final case object HintBase16 extends Tag(23)

  /**
    * The element following this tag is a ByteString
    * containing undecoded CBOR bytes.
    */
  final case object EmbeddedCBOR extends Tag(24)

  /**
    * The element following this tag is a Text element
    * containing a URI as defined in RFC3986.
    *
    * @see https://tools.ietf.org/html/rfc3986
    */
  final case object TextUri extends Tag(32)

  /**
    * The element following this tag is a Text element
    * containing base64url-encoded data.
    *
    * @see https://tools.ietf.org/html/rfc4648
    */
  final case object TextBase64Url extends Tag(33)

  /**
    * The element following this tag is a Text element
    * containing base64-encoded data.
    *
    * @see https://tools.ietf.org/html/rfc4648
    */
  final case object TextBase64 extends Tag(34)

  /**
    * The element following this tag is a Text element
    * containing a regular expression in Perl Compatible Regular
    * Expressions (PCRE) / JavaScript syntax.
    *
    * @see https://tools.ietf.org/html/rfc7049#ref-ECMA262
    */
  final case object TextRegex extends Tag(35)

  /**
    * The element following this tag is a Text element
    * containing a MIME message (including all headers),
    * as defined in RFC2045.
    *
    * @see https://tools.ietf.org/html/rfc2045
    */
  final case object TextMime extends Tag(36)

  /**
    * A "NOP" tag without any semantic meaning whatsoever
    * for the following data item.
    * It does have a somewhat unique wire-format making it
    * suitable as a "magic header" in cases where the content
    * of encoding type of serialized bytes needs to be
    * disambiguated by simply analyzing the bytes themselves.
    */
  final case object MagicHeader extends Tag(55799)

  /**
    * A tag value whose semantics are unknown
    * to this encoder/decoder.
    */
  final case class Other(value: Long) extends Tag(value) {
    internal.Util.requireNonNegative(value, "value")
  }

  /////////////////////////////////////////////////////////////////////////////////////////////////////////

//  implicit val codec = Codec[Tag](_ writeTag _, _.readTag())
}

final case class TaggedValue[T](tag: Tag, value: T)

object TaggedValue {

//  implicit def encoder[T: Encoder]: Encoder[TaggedValue[T]] = Encoder((w, x) => w ~ x.tag ~ x.value)
//  implicit def decoder[T: Decoder]: Decoder[TaggedValue[T]] = Decoder(r => TaggedValue(r.readTag(), r[T]))
}