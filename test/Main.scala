package upickle

import com.fasterxml.jackson.core.JsonParser
import com.fasterxml.jackson.core.`type`.TypeReference
import com.fasterxml.jackson.databind.{DeserializationContext, JsonNode, ObjectMapper}
import com.fasterxml.jackson.databind.deser.std.StdDeserializer
import com.fasterxml.jackson.databind.module.SimpleModule
import com.fasterxml.jackson.databind.node.{IntNode, ObjectNode}
import com.fasterxml.jackson.databind.util.TokenBuffer
import com.fasterxml.jackson.module.scala.DefaultScalaModule
import com.fasterxml.jackson.module.scala.experimental.ScalaObjectMapper


object Main{
  import Generic.ADT
  import Hierarchy._
  import Recursive._
  import Defaults._
  import ADTs.ADT0
  type Data = ADT[Seq[(Int, Int)], String, A, LL, ADTc, ADT0]
  def benchmarkSampledata: Data = ADT(
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
  )

  val duration = 50000
  def main(args: Array[String]): Unit = {
    jacksonModuleScala()
    playJson()
    circe()
    upickleDefault()
    upickleLegacy()
    playJson2()
    circe2()
    upickleDefault2()
    upickleLegacy2()
  }

  def jacksonModuleScala() = {
    val mapper = new ObjectMapper() with ScalaObjectMapper
    val m = new SimpleModule
    mapper.registerModule(DefaultScalaModule)

    // https://stackoverflow.com/questions/47955581/jackson-deserialize-json-to-scala-adt?rq=1
    m.addDeserializer(
      classOf[A],
      new StdDeserializer[A](classOf[A]) {
        def deserialize(jp: JsonParser, ctxt: DeserializationContext): A = {
          val tb = new TokenBuffer(jp, ctxt)
          tb.copyCurrentStructure(jp)
          val firstParser = tb.asParser
          firstParser.nextToken
          val curNode = firstParser.getCodec.readTree[JsonNode](firstParser)
          val objectParser = tb.asParser
          objectParser.nextToken()
          if (curNode.has("i")) {
            objectParser.readValueAs[B](classOf[B])
          } else if (curNode.has("s1")) {
            objectParser.readValueAs[C](classOf[C])
          } else ???
        }
      }
    )
    m.addDeserializer(
      classOf[LL],
      new StdDeserializer[LL](classOf[LL]) {
        def deserialize(jp: JsonParser, ctxt: DeserializationContext): LL = {
          val tb = new TokenBuffer(jp, ctxt)
          tb.copyCurrentStructure(jp)
          val firstParser = tb.asParser
          firstParser.nextToken
          val curNode = firstParser.getCodec.readTree[JsonNode](firstParser)
          val objectParser = tb.asParser
          objectParser.nextToken()
          if (curNode.has("c")) {
            objectParser.readValueAs[Node](classOf[Node])
          } else{
            End
          }
        }
      }
    )
    mapper.registerModule(m)

    val jacksonType = new TypeReference[Data] {}

    val stringified = mapper.writeValueAsString(benchmarkSampledata)
    val r1 = mapper.readValue[Data](stringified, jacksonType)

    assert(benchmarkSampledata == r1)

    val rewritten = mapper.writeValueAsString(mapper.readValue[Data](stringified, jacksonType))
    assert(stringified == rewritten)

    {
      var n = 0
      val start = System.currentTimeMillis()
      while(System.currentTimeMillis() < start + duration){
        mapper.readValue[Data](stringified, classOf[Data])
        n += 1
      }
      println("jacksonModuleScala Read " + n)
    }

    {
      var n = 0
      val start = System.currentTimeMillis()
      while(System.currentTimeMillis() < start + duration){
        mapper.writeValueAsString(benchmarkSampledata)
        n += 1
      }
      println("jacksonModuleScala Write " + n)
    }

  }
  def circe() = {
    import io.circe._, io.circe.parser._, io.circe.generic.semiauto._

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

    val stringified = implicitly[Encoder[Data]].apply(benchmarkSampledata).toString()
    val r1 = decode[Data](stringified).right.get
    assert(benchmarkSampledata == r1)

    val rewritten = implicitly[Encoder[Data]].apply(decode[Data](stringified).right.get).toString()
    assert(stringified == rewritten)

    {
      var n = 0
      val start = System.currentTimeMillis()
      while(System.currentTimeMillis() < start + duration){
        decode[Data](stringified).right.get
        n += 1
      }
      println("circe Read " + n)
    }

    {
      var n = 0
      val start = System.currentTimeMillis()
      while(System.currentTimeMillis() < start + duration){
        implicitly[Encoder[Data]].apply(benchmarkSampledata).toString()
        n += 1
      }
      println("circe Write " + n)
    }

  }

  def playJson() = {
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

    val stringified = Json.stringify(Json.toJson(benchmarkSampledata))
    val r1 = Json.fromJson[Data](Json.parse(stringified)).get
    val equal = benchmarkSampledata == r1
    assert(equal)
    val rewritten = Json.stringify(Json.toJson(Json.fromJson[Data](Json.parse(stringified)).get))
    assert(stringified == rewritten)

    {
      var n = 0
      val start = System.currentTimeMillis()
      while(System.currentTimeMillis() < start + duration){
        Json.fromJson[Data](Json.parse(stringified)).get
        n += 1
      }
      println("playJson Read " + n)
    }

    {
      var n = 0
      val start = System.currentTimeMillis()
      while(System.currentTimeMillis() < start + duration){
        Json.stringify(Json.toJson(benchmarkSampledata))
        n += 1
      }
      println("playJson Write " + n)
    }
  }

  def upickleLegacy() = {
    import upickle.legacy.{macroRW, ReadWriter => RW, Reader => R, Writer => W}

    implicit def rw1: RW[Data] = upickle.legacy.macroRW
    implicit def rw2: RW[A] = upickle.legacy.macroRW
    implicit def rw3: RW[B] = upickle.legacy.macroRW
    implicit def rw4: RW[C] = upickle.legacy.macroRW
    implicit def rw5: RW[LL] = upickle.legacy.macroRW
    implicit def rw6: RW[Node] = upickle.legacy.macroRW
    implicit def rw7: RW[End.type] = upickle.legacy.macroRW
    implicit def rw8: RW[ADTc] = upickle.legacy.macroRW
    implicit def rw9: RW[ADT0] = upickle.legacy.macroRW

    val stringified = upickle.legacy.write(benchmarkSampledata)
    val r1 = upickle.legacy.read[Data](stringified)
    assert(benchmarkSampledata == r1)
    val rewritten = upickle.legacy.write(upickle.legacy.read[Data](stringified))
    assert(stringified == rewritten)

    {
      var n = 0
      val start = System.currentTimeMillis()
      while(System.currentTimeMillis() < start + duration){
        upickle.legacy.read[Data](stringified)
        n += 1
      }
      println("upickleLegacy Read " + n)
    }

    {
      var n = 0
      val start = System.currentTimeMillis()
      while(System.currentTimeMillis() < start + duration){
        upickle.legacy.write(benchmarkSampledata)
        n += 1
      }
      println("upickleLegacy Write " + n)
    }


  }
  def upickleDefault() = {

    val stringified = upickle.default.write(benchmarkSampledata)
    val r1 = upickle.default.read[Data](stringified)
    assert(benchmarkSampledata == r1)
    val rewritten = upickle.default.write(upickle.default.read[Data](stringified))
    assert(stringified == rewritten)

    {
      var n = 0
      val start = System.currentTimeMillis()
      while(System.currentTimeMillis() < start + duration){
        upickle.default.read[Data](stringified)
        n += 1
      }
      println("upickleDefault Read " + n)
    }

    {
      var n = 0
      val start = System.currentTimeMillis()
      while(System.currentTimeMillis() < start + duration){
        upickle.default.write(benchmarkSampledata)
        n += 1
      }
      println("upickleDefault Write " + n)
    }
  }
  def circe2() = {
    import io.circe._, io.circe.parser._, io.circe.generic.semiauto._

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

    val stringified = implicitly[Encoder[Data]].apply(benchmarkSampledata).toString()
    val r1 = decode[Data](stringified).right.get
    assert(benchmarkSampledata == r1)

    val rewritten = implicitly[Encoder[Data]].apply(decode[Data](stringified).right.get).toString()
    assert(stringified == rewritten)

    {
      var n = 0
      val start = System.currentTimeMillis()
      while(System.currentTimeMillis() < start + duration){
        decode[Data](stringified).right.get
        n += 1
      }
      println("circe Read " + n)
    }

    {
      var n = 0
      val start = System.currentTimeMillis()
      while(System.currentTimeMillis() < start + duration){
        implicitly[Encoder[Data]].apply(benchmarkSampledata).toString()
        n += 1
      }
      println("circe Write " + n)
    }

  }

  def playJson2() = {
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

    val stringified = Json.stringify(Json.toJson(benchmarkSampledata))
    val r1 = Json.fromJson[Data](Json.parse(stringified)).get
    val equal = benchmarkSampledata == r1
    assert(equal)
    val rewritten = Json.stringify(Json.toJson(Json.fromJson[Data](Json.parse(stringified)).get))
    assert(stringified == rewritten)

    {
      var n = 0
      val start = System.currentTimeMillis()
      while(System.currentTimeMillis() < start + duration){
        Json.fromJson[Data](Json.parse(stringified)).get
        n += 1
      }
      println("playJson Read " + n)
    }

    {
      var n = 0
      val start = System.currentTimeMillis()
      while(System.currentTimeMillis() < start + duration){
        Json.stringify(Json.toJson(benchmarkSampledata))
        n += 1
      }
      println("playJson Write " + n)
    }
  }

  def upickleLegacy2() = {
    import upickle.legacy.{macroRW, ReadWriter => RW, Reader => R, Writer => W}

    implicit lazy val rw1: RW[Data] = upickle.legacy.macroRW
    implicit lazy val rw2: RW[A] = upickle.legacy.macroRW
    implicit lazy val rw3: RW[B] = upickle.legacy.macroRW
    implicit lazy val rw4: RW[C] = upickle.legacy.macroRW
    implicit lazy val rw5: RW[LL] = upickle.legacy.macroRW
    implicit lazy val rw6: RW[Node] = upickle.legacy.macroRW
    implicit lazy val rw7: RW[End.type] = upickle.legacy.macroRW
    implicit lazy val rw8: RW[ADTc] = upickle.legacy.macroRW
    implicit lazy val rw9: RW[ADT0] = upickle.legacy.macroRW

    val stringified = upickle.legacy.write(benchmarkSampledata)
    val r1 = upickle.legacy.read[Data](stringified)
    assert(benchmarkSampledata == r1)
    val rewritten = upickle.legacy.write(upickle.legacy.read[Data](stringified))
    assert(stringified == rewritten)

    {
      var n = 0
      val start = System.currentTimeMillis()
      while(System.currentTimeMillis() < start + duration){
        upickle.legacy.read[Data](stringified)
        n += 1
      }
      println("upickleLegacy Read " + n)
    }

    {
      var n = 0
      val start = System.currentTimeMillis()
      while(System.currentTimeMillis() < start + duration){
        upickle.legacy.write(benchmarkSampledata)
        n += 1
      }
      println("upickleLegacy Write " + n)
    }


  }
  def upickleDefault2() = {
    implicit lazy val rw1: upickle.default.ReadWriter[Data] = upickle.default.macroRW
    implicit lazy val rw2: upickle.default.ReadWriter[A] = upickle.default.macroRW
    implicit lazy val rw3: upickle.default.ReadWriter[B] = upickle.default.macroRW
    implicit lazy val rw4: upickle.default.ReadWriter[C] = upickle.default.macroRW
    implicit lazy val rw5: upickle.default.ReadWriter[LL] = upickle.default.macroRW
    implicit lazy val rw6: upickle.default.ReadWriter[Node] = upickle.default.macroRW
    implicit lazy val rw7: upickle.default.ReadWriter[End.type] = upickle.default.macroRW
    implicit lazy val rw8: upickle.default.ReadWriter[ADTc] = upickle.default.macroRW
    implicit lazy val rw9: upickle.default.ReadWriter[ADT0] = upickle.default.macroRW
    val stringified = upickle.default.write(benchmarkSampledata)
    val r1 = upickle.default.read[Data](stringified)
    assert(benchmarkSampledata == r1)
    val rewritten = upickle.default.write(upickle.default.read[Data](stringified))
    assert(stringified == rewritten)

    {
      var n = 0
      val start = System.currentTimeMillis()
      while(System.currentTimeMillis() < start + duration){
        upickle.default.read[Data](stringified)
        n += 1
      }
      println("upickleDefault Read " + n)
    }

    {
      var n = 0
      val start = System.currentTimeMillis()
      while(System.currentTimeMillis() < start + duration){
        upickle.default.write(benchmarkSampledata)
        n += 1
      }
      println("upickleDefault Write " + n)
    }
  }
}
