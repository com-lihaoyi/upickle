package upickle

import scala.annotation.switch
import scala.collection.mutable

object Json{
  def write(v: Js.Value): String = v match{
    case Js.String(s) =>
      val out = s.flatMap{
        case '\\' => "\\\\"
        case '"' => "\\\""
        case '/' => "\\/"
        case '\b' => "\\b"
        case '\t' => "\\t"
        case '\n' => "\\n"
        case '\f' => "\\f"
        case '\r' => "\\r"
        case c if c < ' ' =>
          val t ="000" + Integer.toHexString(c)
          "\\u" + t.takeRight(4)
        case c => c.toString
      }
      '"' + out + '"'
    case Js.Object(kv) =>
      val contents = kv.toIterator
        .map{case (k, v) => write(Js.String(k)) + ": " + write(v)}
        .mkString(", ")
      "{" + contents + "}"
    case Js.Array(vs) =>
      "[" + vs.map(write).mkString(", ") + "]"
    case Js.Number(d) => d
    case Js.False => "false"
    case Js.True => "true"
    case Js.Null => "null"
  }
  def read(v: String): Js.Value = JsonParse.parse(v)
}

object Js{
  sealed trait Value
  case class String(s: java.lang.String) extends Value
  case class Object(kv: Seq[(java.lang.String, Value)]) extends Value
  case class Array(args: Seq[Value]) extends Value
  case class Number(d: java.lang.String) extends Value
  case object False extends Value
  case object True extends Value
  case object Null extends Value
}

object JsonParse {

  // *** Character Kinds

  final type CharKind = Int
  final val Letter = 0
  final val Digit = 1
  final val Minus = 2
  final val Quote = 3
  final val Colon = 4
  final val Comma = 5
  final val Lbra = 6
  final val Rbra = 7
  final val Larr = 8
  final val Rarr = 9
  final val Blank = 10
  final val Other = 11
  final val Eof = 12
  final val Slash = 13

  // *** Token Kinds

  final type TokenKind = Int
  final val ID = 0
  final val STRING = 1
  final val NUMBER = 2
  final val BIGNUMBER = 3
  final val FLOATNUMBER = 4
  final val COLON = 5
  final val COMMA = 6
  final val LOBJ = 7
  final val ROBJ = 8
  final val LARR = 9
  final val RARR = 10
  final val BLANK = 11
  final val EOF = 12

  // *** Character => CharKind Map ***

  lazy val charKind = (0 to 255).toArray.map{
    case c if 'a'.toInt <= c && c <= 'z'.toInt => Letter
    case c if 'A'.toInt <= c && c <= 'Z'.toInt => Letter
    case c if '0'.toInt <= c && c <= '9'.toInt => Digit
    case '-' => Minus
    case ',' => Comma
    case '"' => Quote
    case ':' => Colon
    case '{' => Lbra
    case '}' => Rbra
    case '[' => Larr
    case ']' => Rarr
    case ' ' => Blank
    case '\t' => Blank
    case '\n' => Blank
    case '\r' => Blank
    case '/' => Slash
    case _ => Other
  }

  // *** Character Escapes

  final val escapeMap1 = Map[Int, String](
    '\\'.toInt -> "\\",
    '/'.toInt -> "/",
    '\"'.toInt -> "\"",
    'b'.toInt -> "\b",
    'f'.toInt -> "\f",
    'n'.toInt -> "\n",
    'r'.toInt -> "\r",
    't'.toInt -> "\t"
  )

  def parse(s: String): Js.Value = {
    val jp = new JsonParse(s)
    val result = jp.parse()
    result
  }
  class Exception(val msg: String,
                  val input: String,
                  val line: Int,
                  val char: Int)
    extends scala.Exception(s"JsonParse Error: $msg line $line [$char] in $input")
}

class JsonParse(s: String) {

  // *** Import Shared Data ***

  import JsonParse._


  final private[this] val escapeMap = escapeMap1

  // *** INPUT STRING ***

  // array faster than accessing string directly using charAt
  //final private[this] val s1 = s.toCharArray()
  final private[this] val size = s.size

  // *** CHARACTERS ***

  final private[this] var pos = 0

  final private[this] var ch: Int = 0
  final private[this] var chKind: CharKind = 0
  final private[this] var chLinePos: Int = 0
  final private[this] var chCharPos: Int = 0

  final private def chNext {
    if (pos < size) {
      //ch = s1(pos).toInt
      ch = s.charAt(pos)
      chKind = if (ch < 255) {
        charKind(ch)
      } else {
        Other
      }
      pos += 1
      if (ch == '\n'.toInt) {
        chLinePos += 1
        chCharPos = 1
      } else {
        chCharPos += 1
      }
    } else {
      ch = -1
      pos = size + 1
      chKind = Eof
    }
  }


  final private def chError(msg: String): Nothing = {
    throw new JsonParse.Exception(msg, s, chLinePos, chCharPos)
  }

  final private def chMark = pos - 1

  final private def chSubstr(first: Int, delta: Int = 0) = {
    s.substring(first, pos - 1 - delta)
  }

  // *** LEXER ***

  final private[this] var tokenKind: TokenKind = BLANK
  final private[this] var tokenValue: String = ""
  final private[this] var linePos = 1
  final private[this] var charPos = 1

  final private def getDigits() {
    while (chKind == Digit) {
      chNext
    }
  }

  final private def handleDigit() {
    val first = chMark
    getDigits()
    val k1 = if (ch == '.'.toInt) {
      chNext
      getDigits()
      BIGNUMBER
    } else {
      NUMBER
    }
    val k2 = if (ch == 'E'.toInt || ch == 'e'.toInt) {
      chNext
      if (ch == '+'.toInt) {
        chNext
      } else if (ch == '-'.toInt) {
        chNext
      }
      getDigits()
      FLOATNUMBER
    } else {
      k1
    }
    tokenKind = k2
    tokenValue = chSubstr(first)
  }

  final private def handleRaw() {
    chNext
    var first = chMark
    var state = 0
    do {
      if (chKind == Eof) chError("EOF encountered in raw string")
      state = (ch, state) match {
        case ('}', _) => 1
        case ('"', 1) => 2
        case ('"', 2) => 3
        case ('"', 3) => 0
        case _ => 0
      }

      chNext
    } while (state != 3)
    tokenKind = STRING
    tokenValue = chSubstr(first, 3)
  }

  final private def tokenNext {
    do {
      linePos = chLinePos
      charPos = chCharPos
      val kind: Int = chKind
      (kind: @switch) match {
        case Letter => {
          val first = chMark
          while (chKind == Letter || chKind == Digit) {
            chNext
          }
          tokenKind = ID
          tokenValue = chSubstr(first)
        }
        case Digit => handleDigit()
        case Minus => {
          chNext
          handleDigit()
          tokenValue = "-" + tokenValue
        }
        case Quote => {
          var sb: StringBuilder = null
          chNext
          var first = chMark
          while (ch != '"'.toInt && ch >= 32) {
            if (ch == '\\'.toInt) {
              if (sb == null) sb = new StringBuilder(50)
              sb.append(chSubstr(first))
              chNext
              escapeMap.get(ch) match {
                case Some(s) => {
                  sb.append(s)
                  chNext
                }
                case None => {
                  if (ch != 'u'.toInt) chError("Illegal escape")
                  chNext
                  var code = 0
                  for (i <- 1 to 4) {
                    val ch1 = ch.toChar.toString
                    val i = "0123456789abcdef".indexOf(ch1.toLowerCase)
                    if (i == -1) chError("Illegal hex character")
                    code = code * 16 + i
                    chNext
                  }
                  sb.append(code.toChar.toString)
                }
              }
              first = chMark
            } else {
              chNext
            }
          }
          if (ch != '"') chError("Unexpected string character:" + ch.toChar)
          val s1 = chSubstr(first)
          val s2 = if (sb == null) s1
          else {
            sb.append(s1)
            sb.toString
          }
          tokenKind = STRING
          tokenValue = s2
          chNext
          if (s2.length() == 0 && ch == '{') {
            handleRaw()
          }
        }
        case Colon => {
          chNext
          tokenKind = COLON
          tokenValue = ""
        }
        case Comma => {
          chNext
          tokenKind = COMMA
          tokenValue = ""
        }
        case Lbra => {
          chNext
          tokenKind = LOBJ
          tokenValue = ""
        }
        case Rbra => {
          chNext
          tokenKind = ROBJ
          tokenValue = ""
        }
        case Larr => {
          chNext
          tokenKind = LARR
          tokenValue = ""
        }
        case Rarr => {
          chNext
          tokenKind = RARR
          tokenValue = ""
        }
        case Blank => {
          do {
            chNext
          } while (chKind == Blank)
          tokenKind = BLANK
          tokenValue = ""
        }
        case Other => chError("Unexpected character")
        case Eof => {
          chNext
          tokenKind = EOF
          tokenValue = ""
        }
        case Slash => {
          val first = chMark
          if (chKind != Slash) chError("Expecting Slash")
          do {
            chNext
          } while (ch != '\n' && chKind != Eof)
          tokenKind = BLANK
          tokenValue = ""
        }
      }
    } while (tokenKind == BLANK)
  }

  final private def tokenError(msg: String): Nothing = {
    throw new JsonParse.Exception(msg, s, linePos, charPos)
  }

  // *** PARSER ***

  final private def handleEof() = tokenError("Unexpected eof")
  final private def handleUnexpected(i: String) = tokenError(s"Unexpected input: [$i]")

  final private def handleArray(): Js.Array = {
    tokenNext
    var result = List[Js.Value]()
    while (tokenKind != RARR) {
      val t = getJson
      result = t +: result
      if (tokenKind == COMMA) {
        tokenNext
      } else if (tokenKind == RARR) {
      } else {
        tokenError("Expecting , or ]")
      }
    }
    tokenNext
    Js.Array(result.reverse)
  }

  final private def handleObject(): Js.Object = {
    tokenNext
    var result = mutable.Buffer.empty[(String, Js.Value)]
    while (tokenKind != ROBJ) {
      if (tokenKind != STRING && tokenKind != ID) tokenError("Expecting string or name")
      val name = tokenValue
      tokenNext
      if (tokenKind != COLON) tokenError("Expecting :")
      tokenNext
      val t = getJson
      result.append(name -> t)
      if (tokenKind == COMMA) {
        tokenNext
      } else if (tokenKind == ROBJ) {
      } else {
        tokenError("Expecting , or }")
      }
    }
    tokenNext
    Js.Object(result)
  }

  final private def getJson(): Js.Value = {
    val kind: Int = tokenKind
    val result: Js.Value = (kind: @switch) match {
      case ID => {
        val result = tokenValue match {
          case "true" => Js.True
          case "false" => Js.False
          case "null" => Js.Null
          case _ => tokenError("Not true, false, or null")
        }

        tokenNext
        result
      }
      case STRING => {
        val result = tokenValue
        tokenNext
        Js.String(result)
      }
      case NUMBER | BIGNUMBER | FLOATNUMBER => {
        val v = try {
          tokenValue.toLong
        } catch {
          case _: Throwable => tokenError("Bad integer")
        }
        tokenNext
        val r = if (v >= Int.MinValue && v <= Int.MaxValue) v.toInt else v
        Js.Number(""+r)
      }
      case COLON => handleUnexpected(":")
      case COMMA => handleUnexpected(",")
      case LOBJ => handleObject()
      case ROBJ => handleUnexpected("}")
      case LARR => handleArray()
      case RARR => handleUnexpected("]")
      case EOF => handleEof()
    }
    result
  }

  final def parse(): Js.Value = {
    chNext
    tokenNext
    val result = getJson
    if (tokenKind != EOF) tokenError("Excess input")
    result
  }
}




