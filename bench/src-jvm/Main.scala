package upickle

import com.fasterxml.jackson.core.JsonParser
import com.fasterxml.jackson.core.`type`.TypeReference
import com.fasterxml.jackson.databind.deser.std.StdDeserializer
import com.fasterxml.jackson.databind.module.SimpleModule
import com.fasterxml.jackson.databind.util.TokenBuffer
import com.fasterxml.jackson.databind.{DeserializationContext, JsonNode, ObjectMapper}
import com.fasterxml.jackson.module.scala.DefaultScalaModule
import com.fasterxml.jackson.module.scala.experimental.ScalaObjectMapper
import ujson.StringRenderer


object Main{
  import ADTs.ADT0
  import Defaults._
  import Generic.ADT
  import Hierarchy._
  import Recursive._
  def main(args: Array[String]): Unit = {
    for(duration <- Seq(500, 5000, 5000)){
      println("RUN JVM: " + duration)
      println()

//      Main.ujsonAst(duration)
//      Main.upackAst(duration)
//      Main.playJsonAst(duration)
//      Main.uJsonPlayJsonAst(duration)
//      Main.circeJsonAst(duration)
//      Main.uJsonCirceJsonAst(duration)
//      Main.argonautJsonAst(duration)
//      Main.uJsonArgonautJsonAst(duration)
//      Main.json4sJsonAst(duration)
//      Main.uJsonJson4sJsonAst(duration)

//      Main.jacksonModuleScala(duration)
//      Common.playJson(duration)
//      Common.circe(duration)
//      Common.upickleDefault(duration)
//      Common.upickleLegacy(duration)
//      Common.upickleBinaryDefault(duration)
//      Common.upickleBinaryLegacy(duration)
//      Common.genCodec(duration)
//      Common.playJsonCached(duration)
//      Common.circeCached(duration)
//      Common.upickleDefaultCached(duration)
      Common.upickleDefaultCached(duration)
      Common.upickleDefaultCachedReadable(duration)
      Common.upickleDefaultCachedByteArray(duration)
//      Common.upickleLegacyCached(duration)
      Common.upickleDefaultBinaryCached(duration)
      Common.upickleDefaultBinaryCachedReadable(duration)
//      Common.upickleLegacyBinaryCached(duration)
//      Common.genCodecCached(duration)
      println()
    }
  }
  def ujsonAst(duration: Int) = {
    Common.bench0[String, ujson.Value](duration, Common.benchmarkSampleJson)(
      ujson.read(_),
      _.render()
    )
  }
  def upackAst(duration: Int) = {
    Common.bench0[Array[Byte], upack.Msg](duration, Common.benchmarkSampleMsgPack)(
      upack.read(_),
      upack.write(_)
    )
  }
  def playJsonAst(duration: Int) = {
    Common.bench0[String, play.api.libs.json.JsValue](duration, Common.benchmarkSampleJson)(
      play.api.libs.json.Json.parse(_),
      play.api.libs.json.Json.stringify(_)
    )
  }
  def uJsonPlayJsonAst(duration: Int) = {
    Common.bench0[String, play.api.libs.json.JsValue](duration, Common.benchmarkSampleJson)(
      ujson.play.PlayJson(_),
      ujson.play.PlayJson.transform(_, StringRenderer()).toString
    )
  }

  def circeJsonAst(duration: Int) = {
    Common.bench0[String, io.circe.Json](duration, Common.benchmarkSampleJson)(
      io.circe.parser.parse(_).right.get,
      _.toString()
    )
  }
  def uJsonCirceJsonAst(duration: Int) = {
    Common.bench0[String, io.circe.Json](duration, Common.benchmarkSampleJson)(
      ujson.circe.CirceJson(_),
      ujson.circe.CirceJson.transform(_, StringRenderer()).toString
    )
  }

  def argonautJsonAst(duration: Int) = {
    Common.bench0[String, argonaut.Json](duration, Common.benchmarkSampleJson)(
      argonaut.Parse.parse(_).right.get,
      _.toString()
    )
  }
  def uJsonArgonautJsonAst(duration: Int) = {
    Common.bench0[String, argonaut.Json](duration, Common.benchmarkSampleJson)(
      ujson.argonaut.ArgonautJson(_),
      ujson.argonaut.ArgonautJson.transform(_, StringRenderer()).toString
    )
  }
  def json4sJsonAst(duration: Int) = {
    Common.bench0[String, org.json4s.JsonAST.JValue](duration, Common.benchmarkSampleJson)(
      org.json4s.native.JsonMethods.parse(_),
      x => org.json4s.native.JsonMethods.compact(org.json4s.native.JsonMethods.render(x))
    )
  }
  def uJsonJson4sJsonAst(duration: Int) = {
    Common.bench0[String, org.json4s.JsonAST.JValue](duration, Common.benchmarkSampleJson)(
      ujson.json4s.Json4sJson(_),
      ujson.json4s.Json4sJson.transform(_, StringRenderer()).toString
    )
  }
  def jacksonModuleScala(duration: Int) = {
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

    val jacksonType = new TypeReference[Common.Data] {}

    Common.bench[String](duration)(
      mapper.readValue[Seq[Common.Data]](_, jacksonType),
      mapper.writeValueAsString(_)
    )
  }
}
