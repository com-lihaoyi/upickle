package upickle.core

import utest._

object CharBuilderTests extends TestSuite{
  def tests = Tests{

    test("appendSingle") {
      for {
        c0 <- Range.inclusive(Char.MinValue, Char.MaxValue)
        if !Character.isHighSurrogate(c0.toChar) && !Character.isLowSurrogate(c0.toChar)
      } {
        val charBuilder = new upickle.core.CharBuilder
        val byteBuilder = new upickle.core.ByteBuilder
        val stringBuilder = new StringBuilder
        val c = c0.toChar
        charBuilder.appendC(c)
        byteBuilder.appendC(c)
        stringBuilder.append(c)
        val charString = charBuilder.makeString()
        val byteString = byteBuilder.makeString()
        val stringString = stringBuilder.mkString
        if (charString != byteString || charString != stringString){
          throw new Exception(
            s"Inconsistent Character #$c0" +
            "\nbyteString: " + byteString.getBytes.toList +
            "\ncharString" + charString.getBytes.toList +
            "\nstringBuilder: " + stringString.getBytes.toList)
        }
      }
    }
    test("appendSurrogate") {

      for {
        high <- Range.inclusive(Character.MIN_HIGH_SURROGATE, Character.MAX_HIGH_SURROGATE)
        low <- Range.inclusive(Character.MIN_LOW_SURROGATE, Character.MAX_LOW_SURROGATE)
        if Character.isSurrogatePair(high.toChar, low.toChar)
      } {
//        println(s"$high $low")
        val charBuilder = new upickle.core.CharBuilder
        val byteBuilder = new upickle.core.ByteBuilder
        val stringBuilder = new StringBuilder
        charBuilder.appendC(high.toChar)
        charBuilder.appendC(low.toChar)
        byteBuilder.appendC(high.toChar)
        byteBuilder.appendC(low.toChar)
        stringBuilder.append(high.toChar)
        stringBuilder.append(low.toChar)
        val charString = charBuilder.makeString()
        val byteString = byteBuilder.makeString()
        val stringString = stringBuilder.mkString
        if (charString != byteString || charString != stringString){
          throw new Exception(
            s"Inconsistent Surrogate Pair #$high #$low" +
            "\nbyteString: " + byteString.getBytes.toList +
            "\ncharString" + charString.getBytes.toList +
            "\nstringBuilder: " + stringString.getBytes.toList)
        }
//          charBuilder.makeString() ==> stringBuilder.mkString
      }
    }
    test("invalidSurrogate"){
      val byteBuilder = new upickle.core.ByteBuilder
      val ex1 = intercept[Exception]{byteBuilder.appendC(Character.MIN_LOW_SURROGATE)}
      assert(ex1.getMessage == "Un-paired low surrogate 56320")

      byteBuilder.appendC(Character.MIN_HIGH_SURROGATE)
      val ex2 = intercept[Exception]{byteBuilder.appendC(Character.MIN_HIGH_SURROGATE)}
      assert(ex2.getMessage == "Duplicate high surrogate 55296")

      val ex3 = intercept[Exception]{byteBuilder.appendC('a')}
      assert(ex3.getMessage == "Unexpected character following high surrogate 97")
    }
  }
}
