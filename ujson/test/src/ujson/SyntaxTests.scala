package ujson

import java.io.ByteArrayInputStream

import upickle.core.NoOpVisitor

import utest._
import scala.util.Try

object SyntaxTests extends TestSuite{

  import java.nio.ByteBuffer

  def isValidSyntax(s: String): Boolean = {
    val cs = java.nio.CharBuffer.wrap(s.toCharArray)
    val r0 = Try(CharSequenceParser.transform(cs, NoOpVisitor)).isSuccess
    val r1 = Try(StringParser.transform(s, NoOpVisitor)).isSuccess
    val bb = ByteBuffer.wrap(s.getBytes("UTF-8"))
    val r2 = Try(ByteBufferParser.transform(bb, NoOpVisitor)).isSuccess
    val r3 = Try(InputStreamParser.transform(new ByteArrayInputStream(s.getBytes), NoOpVisitor)).isSuccess
    val r4 = Try(ByteArrayParser.transform(s.getBytes, NoOpVisitor)).isSuccess
    val r5 = Try(new InputStreamParser(new ByteArrayInputStream(s.getBytes)).parse(NoOpVisitor)).isSuccess
    if (r0 != r1) sys.error(s"CharSequence/String parsing disagree($r0, $r1): $s")
    if (r1 != r2) sys.error(s"String/ByteBuffer parsing disagree($r1, $r2): $s")
    if (r2 != r3) sys.error(s"ByteBuffer/InputStream parsing disagree($r2, $r3): $s")
    if (r3 != r4) sys.error(s"InputStream/ByteArray parsing disagree($r3, $r4): $s")
    if (r4 != r5) sys.error(s"ByteArray/InputStream5 parsing disagree($r4, $r5): $s")
    r0
  }

  def tests = Tests{
    def qs(s: String): String = "\"" + s + "\""
  
    test("unicode is ok") {
      test{ isValidSyntax(qs("ö")) ==> true }
      test{ isValidSyntax(qs("ö\\\\")) ==> true }
      test{ isValidSyntax(qs("\\\\ö")) ==> true }
    }

    test("true"){
      test{ isValidSyntax("true") ==> true }
      test{ isValidSyntax("tru") ==> false }
      test{ isValidSyntax("truee") ==> false }
    }
    test("false"){
      test{ isValidSyntax("false") ==> true }
      test{ isValidSyntax("fals") ==> false }
      test{ isValidSyntax("falsee") ==> false }
    }

    test("null"){
      test{ isValidSyntax("null") ==> true }
      test{ isValidSyntax("nul") ==> false }
      test{ isValidSyntax("nulll") ==> false }
    }
    test("objects"){
      test{ isValidSyntax("{}") ==> true }
      test{ isValidSyntax("{\"key\": \"value\"}") ==> true }
      test{ isValidSyntax("{}}") ==> false }
      test{ isValidSyntax("{\"key\": \"value\"}}") ==> false }
      test{ isValidSyntax("{{}") ==> false }
      test{ isValidSyntax("{{\"key\": \"value\"}") ==> false }
      test{ isValidSyntax("{{}}") ==> false }
      test{ isValidSyntax("{{\"key\": \"value\"}}") ==> false }
    }
    test("arrays"){
      test{ isValidSyntax("[]") ==> true }
      test{ isValidSyntax("[]]") ==> false }
      test{ isValidSyntax("[[]") ==> false }
      test{ isValidSyntax("[[]]") ==> true }
    }

    test("literal TAB is invalid") { isValidSyntax(qs("\t")) ==> false }
    test("literal NL is invalid") { isValidSyntax(qs("\n")) ==> false }
    test("literal CR is invalid") { isValidSyntax(qs("\r")) ==> false }
    test("literal NUL is invalid") { isValidSyntax(qs("\u0000")) ==> false }
    test("literal BS TAB is invalid") { isValidSyntax(qs("\\\t")) ==> false }
    test("literal BS NL is invalid") { isValidSyntax(qs("\\\n")) ==> false }
    test("literal BS CR is invalid") { isValidSyntax(qs("\\\r")) ==> false }
    test("literal BS NUL is invalid") { isValidSyntax(qs("\\\u0000")) ==> false }
    test("literal BS ZERO is invalid") { isValidSyntax(qs("\\0")) ==> false }
    test("literal BS X is invalid") { isValidSyntax(qs("\\x")) ==> false }
  
    test("0 is ok") { isValidSyntax("0") ==> true }
    test("0e is invalid") { isValidSyntax("0e") ==> false }
    test("123e is invalid") { isValidSyntax("123e") ==> false }
    test(".999 is invalid") { isValidSyntax(".999") ==> false }
    test("0.999 is ok") { isValidSyntax("0.999") ==> true }
    test("-.999 is invalid") { isValidSyntax("-.999") ==> false }
    test("-0.999 is ok") { isValidSyntax("-0.999") ==> true }
    test("+0.999 is invalid") { isValidSyntax("+0.999") ==> false }
    test("--0.999 is invalid") { isValidSyntax("--0.999") ==> false }
    test("01 is invalid") { isValidSyntax("01") ==> false }
    test("1e is invalid") { isValidSyntax("1e") ==> false }
    test("1e- is invalid") { isValidSyntax("1e+") ==> false }
    test("1e+ is invalid") { isValidSyntax("1e-") ==> false }
    test("1. is invalid") { isValidSyntax("1.") ==> false }
    test("1.e is invalid") { isValidSyntax("1.e") ==> false }
    test("1.e9 is invalid") { isValidSyntax("1.e9") ==> false }
    test("1.e- is invalid") { isValidSyntax("1.e+") ==> false }
    test("1.e+ is invalid") { isValidSyntax("1.e-") ==> false }
    test("1.1e is invalid") { isValidSyntax("1.1e") ==> false }
    test("1.1e- is invalid") { isValidSyntax("1.1e-") ==> false }
    test("1.1e+ is invalid") { isValidSyntax("1.1e+") ==> false }
    test("1.1e1 is ok") { isValidSyntax("1.1e1") ==> true }
    test("1.1e-1 is ok") { isValidSyntax("1.1e-1") ==> true }
    test("1.1e+1 is ok") { isValidSyntax("1.1e+1") ==> true }
    test("1+ is invalid") { isValidSyntax("1+") ==> false }
    test("1- is invalid") { isValidSyntax("1-") ==> false }
  
    test("[] is valid") { isValidSyntax("[]") ==> true }
    test("{} is valid") { isValidSyntax("""{"a": true}""") ==> true }
    test("other {} is valid") { isValidSyntax("""{"abc": true}""") ==> true }
    test("duh is valid") { isValidSyntax(""""duh"""") ==> true }
  }
}
