package upickle

object Common{
  import ADTs.ADT0
  import Defaults._
  import Generic.ADT
  import Hierarchy._
  import Recursive._
  type Data = ADT[Seq[(Int, Int)], String, A, LL, ADTc, ADT0]
  val benchmarkSampleData: Seq[Data] = Seq.fill(1000)(ADT(
    Vector((1, 2), (3, 4), (4, 5), (6, 7), (8, 9), (10, 11), (12, 13)),
    """
      |I am cow, hear me moo
      |I weigh twice as much as you
      |And I look good on the barbecueeeee
    """.stripMargin,
    C("lol i am a noob", "haha you are a noob"): A,
    Node(-11, Node(-22, Node(-33, Node(-44, End)))): LL,
    ADTc(i = 1234567890, s = "i am a strange loop"),
    ADT0()
  ))
  val benchmarkSampleJson = upickle.default.write(benchmarkSampleData)
  val benchmarkSampleMsgPack = upickle.default.writeBinary(benchmarkSampleData)

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

    bench[String](duration)(
      decode[Seq[Data]](_).right.get,
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


    bench[String](duration)(
      s => Json.fromJson[Seq[Data]](Json.parse(s)).get,
      d => Json.stringify(Json.toJson(d))
    )
  }

  def upickleLegacy(duration: Int) = {
    import upickle.legacy.{ReadWriter => RW, Reader => R, Writer => W}

    implicit def rw1: RW[Data] = upickle.legacy.macroRW
    implicit def rw2: RW[A] = upickle.legacy.macroRW
    implicit def rw3: RW[B] = upickle.legacy.macroRW
    implicit def rw4: RW[C] = upickle.legacy.macroRW
    implicit def rw5: RW[LL] = upickle.legacy.macroRW
    implicit def rw6: RW[Node] = upickle.legacy.macroRW
    implicit def rw7: RW[End.type] = upickle.legacy.macroRW
    implicit def rw8: RW[ADTc] = upickle.legacy.macroRW
    implicit def rw9: RW[ADT0] = upickle.legacy.macroRW


    bench[String](duration)(
      upickle.legacy.read[Seq[Data]](_),
      upickle.legacy.write(_)
    )


  }
  def upickleDefault(duration: Int) = {

    bench[String](duration)(
      upickle.default.read[Seq[Data]](_),
      upickle.default.write(_)
    )
  }

  def upickleBinaryLegacy(duration: Int) = {
    import upickle.legacy.{ReadWriter => RW, Reader => R, Writer => W}

    implicit def rw1: RW[Data] = upickle.legacy.macroRW
    implicit def rw2: RW[A] = upickle.legacy.macroRW
    implicit def rw3: RW[B] = upickle.legacy.macroRW
    implicit def rw4: RW[C] = upickle.legacy.macroRW
    implicit def rw5: RW[LL] = upickle.legacy.macroRW
    implicit def rw6: RW[Node] = upickle.legacy.macroRW
    implicit def rw7: RW[End.type] = upickle.legacy.macroRW
    implicit def rw8: RW[ADTc] = upickle.legacy.macroRW
    implicit def rw9: RW[ADT0] = upickle.legacy.macroRW


    bench[Array[Byte]](duration)(
      upickle.legacy.readBinary[Seq[Data]](_),
      upickle.legacy.writeBinary(_)
    )


  }
  def upickleBinaryDefault(duration: Int) = {

    bench[Array[Byte]](duration)(
      upickle.default.readBinary[Seq[Data]](_),
      upickle.default.writeBinary(_)
    )
  }

//  def genCodec(duration: Int) = {
//    import com.avsystem.commons.serialization._
//
//    implicit def gc1: GenCodec[Data] = GenCodec.materialize
//    implicit def gc2: GenCodec[A] = GenCodec.materialize
//    implicit def gc3: GenCodec[B] = GenCodec.materialize
//    implicit def gc4: GenCodec[C] = GenCodec.materialize
//    implicit def gc5: GenCodec[LL] = GenCodec.materialize
//    implicit def gc6: GenCodec[Node] = GenCodec.materialize
//    implicit def gc7: GenCodec[End.type] = GenCodec.materialize
//    implicit def gc8: GenCodec[ADTc] = GenCodec.materialize
//    implicit def gc9: GenCodec[ADT0] = GenCodec.materialize
//
//    bench[String](duration)(
//      json.JsonStringInput.read[Data](_),
//      json.JsonStringOutput.write[Data](_)
//    )
//  }

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

    bench[String](duration)(
      decode[Seq[Data]](_).right.get,
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



    bench[String](duration)(
      s => Json.fromJson[Seq[Data]](Json.parse(s)).get,
      d => Json.stringify(Json.toJson(d))
    )

  }

  def upickleLegacyCached(duration: Int) = {
    import upickle.legacy.{ReadWriter => RW, Reader => R, Writer => W}

    implicit lazy val rw1: RW[Data] = upickle.legacy.macroRW
    implicit lazy val rw2: RW[A] = upickle.legacy.macroRW
    implicit lazy val rw3: RW[B] = upickle.legacy.macroRW
    implicit lazy val rw4: RW[C] = upickle.legacy.macroRW
    implicit lazy val rw5: RW[LL] = upickle.legacy.macroRW
    implicit lazy val rw6: RW[Node] = upickle.legacy.macroRW
    implicit lazy val rw7: RW[End.type] = upickle.legacy.macroRW
    implicit lazy val rw8: RW[ADTc] = upickle.legacy.macroRW
    implicit lazy val rw9: RW[ADT0] = upickle.legacy.macroRW

    bench[String](duration)(
      upickle.legacy.read[Seq[Data]](_),
      upickle.legacy.write(_)
    )
  }

  def upickleDefaultCached(duration: Int) = {
    implicit lazy val rw1: upickle.default.ReadWriter[Data] = upickle.default.macroRW
    implicit lazy val rw2: upickle.default.ReadWriter[A] = upickle.default.macroRW
    implicit lazy val rw3: upickle.default.ReadWriter[B] = upickle.default.macroRW
    implicit lazy val rw4: upickle.default.ReadWriter[C] = upickle.default.macroRW
    implicit lazy val rw5: upickle.default.ReadWriter[LL] = upickle.default.macroRW
    implicit lazy val rw6: upickle.default.ReadWriter[Node] = upickle.default.macroRW
    implicit lazy val rw7: upickle.default.ReadWriter[End.type] = upickle.default.macroRW
    implicit lazy val rw8: upickle.default.ReadWriter[ADTc] = upickle.default.macroRW
    implicit lazy val rw9: upickle.default.ReadWriter[ADT0] = upickle.default.macroRW

    bench[String](duration)(
      upickle.default.read[Seq[Data]](_),
      upickle.default.write(_)
    )
  }
  def upickleDefaultCachedByteArray(duration: Int) = {
    implicit lazy val rw1: upickle.default.ReadWriter[Data] = upickle.default.macroRW
    implicit lazy val rw2: upickle.default.ReadWriter[A] = upickle.default.macroRW
    implicit lazy val rw3: upickle.default.ReadWriter[B] = upickle.default.macroRW
    implicit lazy val rw4: upickle.default.ReadWriter[C] = upickle.default.macroRW
    implicit lazy val rw5: upickle.default.ReadWriter[LL] = upickle.default.macroRW
    implicit lazy val rw6: upickle.default.ReadWriter[Node] = upickle.default.macroRW
    implicit lazy val rw7: upickle.default.ReadWriter[End.type] = upickle.default.macroRW
    implicit lazy val rw8: upickle.default.ReadWriter[ADTc] = upickle.default.macroRW
    implicit lazy val rw9: upickle.default.ReadWriter[ADT0] = upickle.default.macroRW

    bench[Array[Byte]](duration)(
      upickle.default.read[Seq[Data]](_),
      upickle.default.write(_).getBytes
    )
  }
  def upickleDefaultCachedReadable(duration: Int) = {
    implicit lazy val rw1: upickle.default.ReadWriter[Data] = upickle.default.macroRW
    implicit lazy val rw2: upickle.default.ReadWriter[A] = upickle.default.macroRW
    implicit lazy val rw3: upickle.default.ReadWriter[B] = upickle.default.macroRW
    implicit lazy val rw4: upickle.default.ReadWriter[C] = upickle.default.macroRW
    implicit lazy val rw5: upickle.default.ReadWriter[LL] = upickle.default.macroRW
    implicit lazy val rw6: upickle.default.ReadWriter[Node] = upickle.default.macroRW
    implicit lazy val rw7: upickle.default.ReadWriter[End.type] = upickle.default.macroRW
    implicit lazy val rw8: upickle.default.ReadWriter[ADTc] = upickle.default.macroRW
    implicit lazy val rw9: upickle.default.ReadWriter[ADT0] = upickle.default.macroRW

    bench[String](duration)(
      x => upickle.default.read[Seq[Data]](x: geny.Readable),
      upickle.default.write(_)
    )
  }

  def upickleLegacyBinaryCached(duration: Int) = {
    import upickle.legacy.{ReadWriter => RW, Reader => R, Writer => W}

    implicit lazy val rw1: RW[Data] = upickle.legacy.macroRW
    implicit lazy val rw2: RW[A] = upickle.legacy.macroRW
    implicit lazy val rw3: RW[B] = upickle.legacy.macroRW
    implicit lazy val rw4: RW[C] = upickle.legacy.macroRW
    implicit lazy val rw5: RW[LL] = upickle.legacy.macroRW
    implicit lazy val rw6: RW[Node] = upickle.legacy.macroRW
    implicit lazy val rw7: RW[End.type] = upickle.legacy.macroRW
    implicit lazy val rw8: RW[ADTc] = upickle.legacy.macroRW
    implicit lazy val rw9: RW[ADT0] = upickle.legacy.macroRW

    bench[Array[Byte]](duration)(
      upickle.legacy.readBinary[Seq[Data]](_),
      upickle.legacy.writeBinary(_)
    )
  }

  def upickleDefaultBinaryCached(duration: Int) = {
    implicit lazy val rw1: upickle.default.ReadWriter[Data] = upickle.default.macroRW
    implicit lazy val rw2: upickle.default.ReadWriter[A] = upickle.default.macroRW
    implicit lazy val rw3: upickle.default.ReadWriter[B] = upickle.default.macroRW
    implicit lazy val rw4: upickle.default.ReadWriter[C] = upickle.default.macroRW
    implicit lazy val rw5: upickle.default.ReadWriter[LL] = upickle.default.macroRW
    implicit lazy val rw6: upickle.default.ReadWriter[Node] = upickle.default.macroRW
    implicit lazy val rw7: upickle.default.ReadWriter[End.type] = upickle.default.macroRW
    implicit lazy val rw8: upickle.default.ReadWriter[ADTc] = upickle.default.macroRW
    implicit lazy val rw9: upickle.default.ReadWriter[ADT0] = upickle.default.macroRW

    bench[Array[Byte]](duration)(
      upickle.default.readBinary[Seq[Data]](_),
      upickle.default.writeBinary(_)
    )
  }
  def upickleDefaultBinaryCachedReadable(duration: Int) = {
    implicit lazy val rw1: upickle.default.ReadWriter[Data] = upickle.default.macroRW
    implicit lazy val rw2: upickle.default.ReadWriter[A] = upickle.default.macroRW
    implicit lazy val rw3: upickle.default.ReadWriter[B] = upickle.default.macroRW
    implicit lazy val rw4: upickle.default.ReadWriter[C] = upickle.default.macroRW
    implicit lazy val rw5: upickle.default.ReadWriter[LL] = upickle.default.macroRW
    implicit lazy val rw6: upickle.default.ReadWriter[Node] = upickle.default.macroRW
    implicit lazy val rw7: upickle.default.ReadWriter[End.type] = upickle.default.macroRW
    implicit lazy val rw8: upickle.default.ReadWriter[ADTc] = upickle.default.macroRW
    implicit lazy val rw9: upickle.default.ReadWriter[ADT0] = upickle.default.macroRW

    bench[Array[Byte]](duration)(
      x => upickle.default.readBinary[Seq[Data]](x: geny.Readable),
      upickle.default.writeBinary(_)
    )
  }

//  def genCodecCached(duration: Int) = {
//    import com.avsystem.commons.serialization._
//
//    implicit lazy val gc1: GenCodec[Data] = GenCodec.materialize
//    implicit lazy val gc2: GenCodec[A] = GenCodec.materialize
//    implicit lazy val gc3: GenCodec[B] = GenCodec.materialize
//    implicit lazy val gc4: GenCodec[C] = GenCodec.materialize
//    implicit lazy val gc5: GenCodec[LL] = GenCodec.materialize
//    implicit lazy val gc6: GenCodec[Node] = GenCodec.materialize
//    implicit lazy val gc7: GenCodec[End.type] = GenCodec.materialize
//    implicit lazy val gc8: GenCodec[ADTc] = GenCodec.materialize
//    implicit lazy val gc9: GenCodec[ADT0] = GenCodec.materialize
//
//    bench[String](duration)(
//      json.JsonStringInput.read[Data](_),
//      json.JsonStringOutput.write[Data](_)
//    )
//  }

  def bench[T](duration: Int)
              (f1: T => Seq[Data], f2: Seq[Data] => T)
              (implicit name: sourcecode.Name) = {
    val stringified = f2(benchmarkSampleData)
    val r1 = f1(stringified)
    val equal = benchmarkSampleData == r1

    assert(equal)
    val rewritten = f2(f1(stringified))
    (stringified, rewritten) match{
      case (lhs: Array[_], rhs: Array[_]) => assert(lhs.toSeq == rhs.toSeq)
      case _ => assert(stringified == rewritten)
    }
    bench0[T, Seq[Data]](duration, stringified)(f1, f2)
  }

  def bench0[T, V](duration: Int, stringified: T)
                  (f1: T => V, f2: V => T)
                  (implicit name: sourcecode.Name)= {
    {
      var n = 0
      val start = System.currentTimeMillis()
      while(System.currentTimeMillis() < start + duration){
        f1(stringified)
        n += 1
      }
      println(name.value + " Read " + n)
    }

    val parsed = f1(stringified)

    {
      var n = 0
      val start = System.currentTimeMillis()
      while(System.currentTimeMillis() < start + duration){
        f2(parsed)
        n += 1
      }
      println(name.value + " Write " + n)
    }
  }
}
