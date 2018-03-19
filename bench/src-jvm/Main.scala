package upickle

import com.fasterxml.jackson.core.JsonParser
import com.fasterxml.jackson.core.`type`.TypeReference
import com.fasterxml.jackson.databind.deser.std.StdDeserializer
import com.fasterxml.jackson.databind.module.SimpleModule
import com.fasterxml.jackson.databind.util.TokenBuffer
import com.fasterxml.jackson.databind.{DeserializationContext, JsonNode, ObjectMapper}
import com.fasterxml.jackson.module.scala.DefaultScalaModule
import com.fasterxml.jackson.module.scala.experimental.ScalaObjectMapper


object Main{

  def main(args: Array[String]): Unit = {
    for(duration <- Seq(5000, 50000)){
      println("RUN JVM: " + duration)
      println()
      Main.jacksonModuleScala(duration)
      Common.playJson(duration)
      Common.circe(duration)
      Common.upickleDefault(duration)
      Common.upickleLegacy(duration)
      Common.playJsonCached(duration)
      Common.circeCached(duration)
      Common.upickleDefaultCached(duration)
      Common.upickleLegacyCached(duration)
      println()
    }
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

    val jacksonType = new TypeReference[Data] {}

    bench("jackson", duration)(
      mapper.readValue[Data](_, jacksonType),
      mapper.writeValueAsString(_)
    )
  }

}
