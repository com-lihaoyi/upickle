package upickle

import upack.MsgPackWriter

import scala.annotation.nowarn

object Main{
  import ADTs.ADT0
  import Defaults._
  import Generic.ADT
  import Hierarchy._
  import Recursive._
  def main(args: Array[String]): Unit = {
    val allResults = collection.mutable.Buffer.empty[(String, Int)]
    for((duration, save) <- Seq(2500 -> false, 5000 -> false, 10000 -> true, 10000 -> true, 10000 -> true)){
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


      val results = Seq(
//        NonNative.playJson(duration),
//        NonNative.circe(duration),
        Common.upickleDefault(duration),
        Common.upickleDefaultByteArray(duration),
        Common.upickleDefaultBinary(duration),
        //      Common.upickleLegacy(duration)
        //      Common.upickleBinaryLegacy(duration)
        //      Common.genCodec(duration)
  //      NonNative.playJsonCached(duration)
  //      NonNative.circeCached(duration)
  //      Common.upickleDefaultCached(duration)
  //      Common.upickleDefaultByteArrayCached(duration)
  //      Common.upickleDefaultBinaryCached(duration)
        //      Common.upickleDefaultCachedReadable(duration)
        //      Common.upickleDefaultCachedReadablePath(duration)

        //      Common.upickleLegacyCached(duration)
        //      Common.upickleDefaultBinaryCachedReadable(duration)
        //      Common.upickleLegacyBinaryCached(duration)
        //      Common.genCodecCached(duration)
  //      benchParsingRendering(duration, bytes = true, strings = false, streams = false, msgpack = false)
  //      benchParsingRendering(duration, bytes = false, strings = true, streams = false, msgpack = false)
  //      benchParsingRendering(duration, bytes = false, strings = false, streams = true, msgpack = false)
  //      benchParsingRendering(duration, bytes = false, strings = false, streams = false, msgpack = true)
        Common.integers(duration),
        Common.integersByteArray(duration),
        Common.integersBinary(duration),

        Common.doubles(duration),
        Common.doublesByteArray(duration),
        Common.doublesBinary(duration),

        Common.shortStrings(duration),
        Common.shortStringsByteArray(duration),
        Common.shortStringsBinary(duration),

        Common.longStrings(duration),
        Common.longStringsByteArray(duration),
        Common.longStringsBinary(duration),

        Common.unicodeStrings(duration),
        Common.unicodeStringsByteArray(duration),
        Common.unicodeStringsBinary(duration),

        Common.caseClasses(duration),
        Common.caseClassesByteArray(duration),
        Common.caseClassesBinary(duration),

        Common.sequences(duration),
        Common.sequencesByteArray(duration),
        Common.sequencesBinary(duration),
      )

      if (save) allResults.appendAll(results.flatten)
      println()
    }
    val prettyResults = Common.prettyPrintResults(allResults)
    println(prettyResults)
    for(savePath <- args.headOption){
      val p = java.nio.file.Paths.get(savePath)
      if (!java.nio.file.Files.exists(p)) java.nio.file.Files.write(p, prettyResults.getBytes)
      else {
        val before = new String(java.nio.file.Files.readAllBytes(p))
        def split(s: String) = s.linesIterator.map(_.drop(2).dropRight(2).split(" *\\| *")).toList
        println(
          split(prettyResults).zip(split(before)).zipWithIndex.map {
            case ((afterLine, beforeLine), 0 | 1) => afterLine
            case ((afterLine, beforeLine), _) =>
              afterLine.zip(beforeLine) match {
                case Array(head, tail@_*) =>
                  Array(head._1) ++ tail.map { case (a, b) =>
                    val percentChange = ((a.toDouble / b.toDouble - 1) * 100).toInt
                    val sign =
                      if (percentChange == 0) "" else if (percentChange < 0) "-" else if (percentChange > 0) "+"
                    s"$b -> $a (${sign}${math.abs(percentChange)}%)"
                  }
              }
          }.map(_.mkString("| ", " | ", " |")).mkString("\n")
        )
      }
    }
  }

  @nowarn("cat=deprecation")
  def benchParsingRendering(duration: Int, bytes: Boolean, strings: Boolean, streams: Boolean, msgpack: Boolean) = {
    import java.nio.file.{Files, Paths}
    import collection.JavaConverters._
    val names = Files.list(Paths.get("exampleJson")).iterator().asScala.toArray
    val inputByteArrays = for(name <- names) yield Files.readAllBytes(name)
    lazy val inputStrings = for(inputByteArray <- inputByteArrays) yield new String(inputByteArray)
    lazy val inputMsgPacks = for(inputByteArray <- inputByteArrays) yield ujson.transform(new String(inputByteArray), upack.Msg)

    {
      var n = 0
      val start = System.currentTimeMillis()
      while(System.currentTimeMillis() < start + duration){
        if (bytes) {
          for (inputByteArray <- inputByteArrays) {
            ujson.reformatToOutputStream(inputByteArray, new java.io.ByteArrayOutputStream())
          }
        }
        if(strings){
          for((inputString, i) <- inputStrings.zipWithIndex) {
            ujson.reformatTo(inputString, new java.io.StringWriter())
          }
        }
        if(streams){
          for (inputByteArray <- inputByteArrays) {
            ujson.reformatToOutputStream(
              new java.io.ByteArrayInputStream(inputByteArray),
              new java.io.ByteArrayOutputStream()
            )
          }
        }
        if(msgpack){
          for (inputMsgPack <- inputMsgPacks) {
            upack.transform(inputMsgPack, new MsgPackWriter())
          }
        }

        n += 1
      }
      val bytesPrefix = if (bytes) "Bytes " else ""
      val stringPrefix = if (strings) "String " else ""
      val streamPrefix = if (streams) "Stream " else ""
      val msgpackPrefix = if (msgpack) "MsgPack " else ""
      println(streamPrefix + bytesPrefix + stringPrefix + msgpackPrefix + "Parsing Rendering  " + n)
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
      ujson.play.PlayJson.transform(_, ujson.StringRenderer()).toString
    )
  }

  def circeJsonAst(duration: Int) = {
    Common.bench0[String, io.circe.Json](duration, Common.benchmarkSampleJson)(
      io.circe.parser.parse(_).getOrElse(???),
      _.toString()
    )
  }
  def uJsonCirceJsonAst(duration: Int) = {
    Common.bench0[String, io.circe.Json](duration, Common.benchmarkSampleJson)(
      ujson.circe.CirceJson(_),
      ujson.circe.CirceJson.transform(_, ujson.StringRenderer()).toString
    )
  }

  def argonautJsonAst(duration: Int) = {
    Common.bench0[String, argonaut.Json](duration, Common.benchmarkSampleJson)(
      argonaut.Parse.parse(_).getOrElse(???),
      _.toString()
    )
  }
  def uJsonArgonautJsonAst(duration: Int) = {
    Common.bench0[String, argonaut.Json](duration, Common.benchmarkSampleJson)(
      ujson.argonaut.ArgonautJson(_),
      ujson.argonaut.ArgonautJson.transform(_, ujson.StringRenderer()).toString
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
      ujson.json4s.Json4sJson.transform(_, ujson.StringRenderer()).toString
    )
  }
}
