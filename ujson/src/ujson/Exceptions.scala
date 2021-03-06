package ujson


sealed trait ParsingFailedException extends Exception

case class ParseException(clue: String, index: Int, line: Int, col: Int)
  extends Exception(clue + " at index " + index) with ParsingFailedException

case class IncompleteParseException(msg: String)
  extends Exception(msg) with ParsingFailedException
