package ujson
import java.nio.charset.StandardCharsets
import utest._
object SmallJsonTests extends TestSuite {
  val tests = Tests {
    test("unicode") {
      val unparsed = "\"\\u00a0\""
      val fromString = ujson.read(unparsed).render(indent = 4)
      val fromBytes = ujson.read(unparsed.getBytes(StandardCharsets.UTF_8)).render(indent = 4)
      assert(fromString == fromBytes)
    }
    test("shortcuts"){
      test("positive"){
        ujson.read("[1]").arr        ==> Seq(ujson.Num(1))
        ujson.read("1").num          ==> 1
        ujson.read("\"1\"").str      ==> "1"
        ujson.read("{\"1\": 1}").obj ==> Map("1" -> ujson.Num(1))
      }
      test("negative"){
        intercept[ujson.Value.InvalidData]{ujson.read("[1]").obj}
        intercept[ujson.Value.InvalidData]{ujson.read("1").obj}
        intercept[ujson.Value.InvalidData]{ujson.read("\"1\"").obj}

      }
    }
    test("inputsEu"){
      TestUtil.checkParse(
        """{
          |  "count": 12129,
          |  "facets": {},
          |  "results": [
          |    {
          |      "activity_consult_committees": null,
          |      "activity_high_level_groups": null,
          |      "head_office_lat": null,
          |      "updated_at": "2019-02-26T00:53:13.297966",
          |      "entity": "f68eebed3ca14d66aeb9be6e5680cdcd",
          |      "number_of_natural_persons": null,
          |      "legal": "8e8b73cc70d241e5bb32c8907cd042ba",
          |      "native_name": null,
          |      "head_office_country": "Denmark",
          |      "id": "fffebd3272294bb0a38d0347b0e0c4df",
          |      "activity_industry_forums": "None",
          |      "contact_country": 59,
          |      "head_office_postbox": null,
          |      "networking": "The European Federation of Building and Woodworkers (EFBWW) is the European Industry Federation for the construction industry, the building materials industry, the wood and furniture industry and the forestry industry. The EFBWW has 76 affiliated unions in 34 countries and represents a total of 2,000,000 members, see\r\nhttp://www.efbww.org/default.asp?Language=EN",
          |      "members_75": null,
          |      "main_category": 2,
          |      "members_50": 4,
          |      "activity_expert_groups": "none",
          |      "other_code_of_conduct": null,
          |      "head_office_town": "København V"
          |    }
          |
          |
          |  ]
          |}""".stripMargin,
        true
      )
    }
  }
}
