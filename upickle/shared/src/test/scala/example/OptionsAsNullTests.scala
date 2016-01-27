package example
import acyclic.file
import upickle.{Js, TestUtil}
import utest._

import example.Simple.Thing

  case class Opt(a:Option[String], b:Option[Int])


object OptionsAsNullTests extends TestSuite{

  import TestUtil._
  val tests = TestSuite{
    'nullAsNone{
      object OptionPickle extends upickle.AttributeTagged{

        override def OptionW[T: Writer]: Writer[Option[T]] = Writer{
          case None => Js.Null
          case Some(s) => implicitly[Writer[T]].write(s)
        }

        override def OptionR[T: Reader]: Reader[Option[T]] = Reader {
          case Js.Null => None
          case v:Js.Value => Some(implicitly[Reader[T]].read.apply(v))
        }
      }
      
      // Quick check to ensure we didn't break anything
      'primitive{
	      OptionPickle.write("A String") --> "\"A String\""
	      OptionPickle.read[String]("\"A String\"") --> "A String"
	      OptionPickle.write(1) --> "1"
	      OptionPickle.read[Int]("1") --> 1
	      OptionPickle.write(Thing(1, "gg")) --> """{"myFieldA":1,"myFieldB":"gg"}"""
	      OptionPickle.read[Thing]("""{"myFieldA":1,"myFieldB":"gg"}""") --> Thing(1, "gg")
  	  }

  	  'none{
	      OptionPickle.write(None) --> "null"
    	  OptionPickle.read[None.type]("null") --> None
  	  }

  	  'some{
	      OptionPickle.write(Some("abc")) --> "\"abc\""
	      OptionPickle.read[Some[String]]("\"abc\"") --> Some("abc")
	      OptionPickle.write(Some(1)) --> "1"
	      OptionPickle.read[Some[Int]]("1") --> Some(1)
	      OptionPickle.write(Some(3.14159)) --> "3.14159"
	      OptionPickle.read[Some[Double]]("3.14159") --> Some(3.14159)
  	  }

  	  'option{
  	  	  import OptionPickle._
  	  	  write(Option("abc"))	--> "\"abc\""
  	  	  read[Option[String]]("\"abc\"") --> Some("abc")
  	  }

  	  'caseClass{
  	  	  import OptionPickle._

		  write(Opt(None, None)) --> """{"a":null,"b":null}"""  	  	
      	  read[Opt]("""{"a":null,"b":null}""") --> Opt(None, None)
          write(Opt(Some("abc"), Some(1))) --> """{"a":"abc","b":1}"""
  	  }

  	  'optionCaseClass{
  	  	  import OptionPickle._
  	  	  implicit val thingReader = implicitly[Reader[Thing]]
  	  	  implicit val thingWriter = implicitly[Writer[Thing]]

		  write(Opt(None, None)) --> """{"a":null,"b":null}"""  	  	
      	  read[Opt]("""{"a":null,"b":null}""") --> Opt(None, None)
          write(Opt(Some("abc"), Some(1))) --> """{"a":"abc","b":1}"""

          write(Option(Thing(1, "gg"))) --> """{"myFieldA":1,"myFieldB":"gg"}"""
          read[Option[Thing]]("""{"myFieldA":1,"myFieldB":"gg"}""") --> Option(Thing(1, "gg"))
  	  }

    }
  }
}