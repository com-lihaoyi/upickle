package ujson

import utest._

object ExampleJsonTests extends TestSuite {
  def check(name: String) = {
    import java.nio.file.{Files, Paths}
    val byteArray = Files.readAllBytes(Paths.get("exampleJson", name))
    val string = new String(byteArray)
    val stream = new java.io.ByteArrayInputStream(byteArray)

    val arrayJson = ujson.read(byteArray)
    val stringJson = ujson.read(string)
    val streamJson = ujson.read(stream)
    assert(arrayJson == stringJson)
    assert(arrayJson == streamJson)

    val stringWriter = new java.io.StringWriter()
    ujson.writeTo(arrayJson, stringWriter)
    val bytesOut = new java.io.ByteArrayOutputStream()
    ujson.writeToOutputStream(arrayJson, bytesOut)
    assert(stringWriter.toString == new String(bytesOut.toByteArray))
  }
  val tests = Tests {
    test - check("australia-abc.json")
    test - check("bitcoin.json")
    test - check("doj-blog.json")
    test - check("eu-lobby-country.json")
    test - check("eu-lobby-financial.json")
    test - check("eu-lobby-repr.json")
    test - check("github-events.json")
    test - check("github-gists.json")
    test - check("json-generator.json")
    test - check("meteorites.json")
    test - check("movies.json")
    test - check("reddit-scala.json")
    test - check("rick-morty.json")
    test - check("temp-anomaly.json")
    test - check("thai-cinemas.json")
    test - check("turkish.json")
    test - check("twitter_api_compact_response.json")
    test - check("twitter_api_response.json")
  }
}
