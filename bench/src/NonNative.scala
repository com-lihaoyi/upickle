package upickle
object NonNative{
  import Common._
  import ADTs.ADT0
  import Defaults._
  import Generic.ADT
  import Hierarchy._
  import Recursive._
  def circe(duration: Int) = {
    import io.circe._
    import io.circe.generic.semiauto._
    import io.circe.parser._

    implicit def _r1: Decoder[Data] = deriveDecoder
    implicit def _r2: Decoder[A] = deriveDecoder
    implicit def _r3: Decoder[B] = deriveDecoder
    implicit def _r4: Decoder[C] = deriveDecoder
    implicit def _r5: Decoder[LL] = deriveDecoder
    implicit def _r6: Decoder[Node] = deriveDecoder
    implicit def _r7: Decoder[End.type] = deriveDecoder
    implicit def _r8: Decoder[ADTc] = deriveDecoder
    implicit def _r9: Decoder[ADT0] = deriveDecoder

    implicit def _w1: Encoder[Data] = deriveEncoder
    implicit def _w2: Encoder[A] = deriveEncoder
    implicit def _w3: Encoder[B] = deriveEncoder
    implicit def _w4: Encoder[C] = deriveEncoder
    implicit def _w5: Encoder[LL] = deriveEncoder
    implicit def _w6: Encoder[Node] = deriveEncoder
    implicit def _w7: Encoder[End.type] = deriveEncoder
    implicit def _w8: Encoder[ADTc] = deriveEncoder
    implicit def _w9: Encoder[ADT0] = deriveEncoder

    Common.bench[String](duration)(
      decode[Seq[Data]](_).getOrElse(???),
      implicitly[Encoder[Seq[Data]]].apply(_).toString()
    )

  }

  def playJson(duration: Int) = {
    import play.api.libs.json._
    implicit def rw1: Format[Data] = play.api.libs.json.Json.format
    implicit def rw2: Format[A] = play.api.libs.json.Json.format
    implicit def rw3: Format[B] = play.api.libs.json.Json.format
    implicit def rw4: Format[C] = play.api.libs.json.Json.format
    implicit def rw5: Format[LL] = play.api.libs.json.Json.format
    implicit def rw6: Format[Node] = play.api.libs.json.Json.format
    implicit def rw7: Format[End.type] = new Format[End.type] {
      def reads(json: JsValue) = JsSuccess(End)

      def writes(o: Recursive.End.type) = JsObject(Nil)
    }
    implicit def rw8: Format[ADTc] = play.api.libs.json.Json.format
    implicit def rw9: Format[ADT0] = new Format[ADT0] {
      def reads(json: JsValue) = JsSuccess(ADT0())

      def writes(o: ADT0) = JsObject(Nil)
    }


    Common.bench[String](duration)(
      s => Json.fromJson[Seq[Data]](Json.parse(s)).get,
      d => Json.stringify(Json.toJson(d))
    )
  }


  def circeCached(duration: Int) = {
    import io.circe._
    import io.circe.generic.semiauto._
    import io.circe.parser._

    implicit lazy val _r1: Decoder[Data] = deriveDecoder
    implicit lazy val _r2: Decoder[A] = deriveDecoder
    implicit lazy val _r3: Decoder[B] = deriveDecoder
    implicit lazy val _r4: Decoder[C] = deriveDecoder
    implicit lazy val _r5: Decoder[LL] = deriveDecoder
    implicit lazy val _r6: Decoder[Node] = deriveDecoder
    implicit lazy val _r7: Decoder[End.type] = deriveDecoder
    implicit lazy val _r8: Decoder[ADTc] = deriveDecoder
    implicit lazy val _r9: Decoder[ADT0] = deriveDecoder

    implicit lazy val _w1: Encoder[Data] = deriveEncoder
    implicit lazy val _w2: Encoder[A] = deriveEncoder
    implicit lazy val _w3: Encoder[B] = deriveEncoder
    implicit lazy val _w4: Encoder[C] = deriveEncoder
    implicit lazy val _w5: Encoder[LL] = deriveEncoder
    implicit lazy val _w6: Encoder[Node] = deriveEncoder
    implicit lazy val _w7: Encoder[End.type] = deriveEncoder
    implicit lazy val _w8: Encoder[ADTc] = deriveEncoder
    implicit lazy val _w9: Encoder[ADT0] = deriveEncoder

    Common.bench[String](duration)(
      decode[Seq[Data]](_).getOrElse(???),
      implicitly[Encoder[Seq[Data]]].apply(_).toString()
    )
  }

  def playJsonCached(duration: Int) = {
    import play.api.libs.json._
    implicit lazy val rw1: Format[Data] = play.api.libs.json.Json.format
    implicit lazy val rw2: Format[A] = play.api.libs.json.Json.format
    implicit lazy val rw3: Format[B] = play.api.libs.json.Json.format
    implicit lazy val rw4: Format[C] = play.api.libs.json.Json.format
    implicit lazy val rw5: Format[LL] = play.api.libs.json.Json.format
    implicit lazy val rw6: Format[Node] = play.api.libs.json.Json.format
    implicit lazy val rw7: Format[End.type] = new Format[End.type] {
      def reads(json: JsValue) = JsSuccess(End)

      def writes(o: Recursive.End.type) = JsObject(Nil)
    }
    implicit lazy val rw8: Format[ADTc] = play.api.libs.json.Json.format
    implicit lazy val rw9: Format[ADT0] = new Format[ADT0] {
      def reads(json: JsValue) = JsSuccess(ADT0())

      def writes(o: ADT0) = JsObject(Nil)
    }



    Common.bench[String](duration)(
      s => Json.fromJson[Seq[Data]](Json.parse(s)).get,
      d => Json.stringify(Json.toJson(d))
    )

  }
}
