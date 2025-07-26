package ujson

import utest._

object SyntaxTests extends TestSuite{


  def tests = Tests{
    def qs(s: String): String = "\"" + s + "\""


    test("empty"){
      test{ TestUtil.checkParse("", false) }
      test{ TestUtil.checkParse(" ", false) }
    }
    test("unicode is ok") {
      test{ TestUtil.checkParse(qs("ö"), true) }
      test{ TestUtil.checkParse(qs("ö\\\\"), true) }
      test{ TestUtil.checkParse(qs("\\\\ö"), true) }
    }

    test("true"){
      test{ TestUtil.checkParse("true", true) }
      test{ TestUtil.checkParse("tru", false) }
      test{ TestUtil.checkParse("truee", false) }
    }
    test("false"){
      test{ TestUtil.checkParse("false", true) }
      test{ TestUtil.checkParse("fals", false) }
      test{ TestUtil.checkParse("falsee", false) }
    }

    test("null"){
      test{ TestUtil.checkParse("null", true) }
      test{ TestUtil.checkParse("nul", false) }
      test{ TestUtil.checkParse("nulll", false) }
    }

    test("string"){
      test{ TestUtil.checkParse("\"쫾\"", true) }
      test{ TestUtil.checkParse("\"\"", true) }
      test{ TestUtil.checkParse("\"i am cow hear me moo\"", true) }
      test{ TestUtil.checkParse("\" \\r \\n \\f \\t \\b \\\" \\\\ \\/ \"", true) }
      test{ TestUtil.checkParse("\" \\u53c9\\u70e7\\u5305 \"", true) }
      test{ TestUtil.checkParse("\" 叉烧包 \"", true) }
      test{ TestUtil.checkParse("\" \\k \"", false) }
      test{ TestUtil.checkParse("\" \n \"", false) }
      test{ TestUtil.checkParse("\" \t \"", false) }
      test{ TestUtil.checkParse("\"", false) }
      test("duh is valid") { TestUtil.checkParse(""""duh"""", true) }

      test("literal TAB is invalid") { TestUtil.checkParse(qs("\t"), false) }
      test("literal NL is invalid") { TestUtil.checkParse(qs("\n"), false) }
      test("literal CR is invalid") { TestUtil.checkParse(qs("\r"), false) }
      test("literal NUL is invalid") { TestUtil.checkParse(qs("\u0000"), false) }
      test("literal BS TAB is invalid") { TestUtil.checkParse(qs("\\\t"), false) }
      test("literal BS NL is invalid") { TestUtil.checkParse(qs("\\\n"), false) }
      test("literal BS CR is invalid") { TestUtil.checkParse(qs("\\\r"), false) }
      test("literal BS NUL is invalid") { TestUtil.checkParse(qs("\\\u0000"), false) }
      test("literal BS ZERO is invalid") { TestUtil.checkParse(qs("\\0"), false) }
      test("literal BS X is invalid") { TestUtil.checkParse(qs("\\x"), false) }
    }
    test("objects"){
      test{ TestUtil.checkParse("{}", true) }
      test{ TestUtil.checkParse("{\"key\": \"value\"}", true) }
      test{ TestUtil.checkParse("{}}", false) }
      test{ TestUtil.checkParse("{\"key\": \"value\"}}", false) }
      test{ TestUtil.checkParse("{{}", false) }
      test{ TestUtil.checkParse("{{\"key\": \"value\"}", false) }
      test{ TestUtil.checkParse("{{}}", false) }
      test{ TestUtil.checkParse("{{\"key\": \"value\"}}", false) }

    }
    test("arrays"){
      test{ TestUtil.checkParse("[]", true) }
      test{ TestUtil.checkParse("[]]", false) }
      test{ TestUtil.checkParse("[[]", false) }
      test{ TestUtil.checkParse("[[]]", true) }
    }

    test("numbers"){

      test("0 is ok") { TestUtil.checkParse("0", true) }
      test("0e is invalid") { TestUtil.checkParse("0e", false) }
      test("123e is invalid") { TestUtil.checkParse("123e", false) }
      test(".999 is invalid") { TestUtil.checkParse(".999", false) }
      test("0.999 is ok") { TestUtil.checkParse("0.999", true) }
      test("-.999 is invalid") { TestUtil.checkParse("-.999", false) }
      test("-0.999 is ok") { TestUtil.checkParse("-0.999", true) }
      test("+0.999 is invalid") { TestUtil.checkParse("+0.999", false) }
      test("--0.999 is invalid") { TestUtil.checkParse("--0.999", false) }
      test("01 is invalid") { TestUtil.checkParse("01", false) }
      test("1e is invalid") { TestUtil.checkParse("1e", false) }
      test("1e- is invalid") { TestUtil.checkParse("1e+", false) }
      test("1e+ is invalid") { TestUtil.checkParse("1e-", false) }
      test("1. is invalid") { TestUtil.checkParse("1.", false) }
      test("1.e is invalid") { TestUtil.checkParse("1.e", false) }
      test("1.e9 is invalid") { TestUtil.checkParse("1.e9", false) }
      test("1.e- is invalid") { TestUtil.checkParse("1.e+", false) }
      test("1.e+ is invalid") { TestUtil.checkParse("1.e-", false) }
      test("1.1e is invalid") { TestUtil.checkParse("1.1e", false) }
      test("1.1e- is invalid") { TestUtil.checkParse("1.1e-", false) }
      test("1.1e+ is invalid") { TestUtil.checkParse("1.1e+", false) }
      test("1.1e1 is ok") { TestUtil.checkParse("1.1e1", true) }
      test("1.1e-1 is ok") { TestUtil.checkParse("1.1e-1", true) }
      test("1.1e+1 is ok") { TestUtil.checkParse("1.1e+1", true) }
      test("1+ is invalid") { TestUtil.checkParse("1+", false) }
      test("1- is invalid") { TestUtil.checkParse("1-", false) }
    }

    test("whitespace") {
      test { TestUtil.checkParse("\n{\n}", true) }
      test { TestUtil.checkParse("\r{\r}", true) }
      test { TestUtil.checkParse("\r\n{\r\n}", true) }
      test { TestUtil.checkParse("r{}", false) }
      test { TestUtil.checkParse("n{}", false) }
    }
  }
}
