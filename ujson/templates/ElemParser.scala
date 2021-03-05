package ujson
import java.io.StringWriter

import upickle.core.{Abort, AbortException, ObjArrVisitor, ObjVisitor, Visitor}
import java.nio.charset.Charset


import scala.annotation.{switch, tailrec}

/**
 * Parser implements a state machine for correctly parsing JSON data.
 *
 * The trait relies on a small number of methods which are left
 * abstract, and which generalize parsing based on whether the input
 * is in Bytes or Chars, coming from Strings, files, or other input.
 * All methods provided here are protected, so different parsers can
 * choose which functionality to expose.
 *
 * Parser is parameterized on J, which is the type of the JSON AST it
 * will return. Jawn can produce any AST for which a Facade[J] is
 * available.
 *
 * The parser trait does not hold any state itself, but particular
 * implementations will usually hold state. Parser instances should
 * not be reused between parsing runs.
 *
 * For now the parser requires input to be in UTF-8. This requirement
 * may eventually be relaxed.
 */
abstract class ElemParser[J]{
  val elemOps = ujson.util.ElemOps
  val outputBuilder = new ujson.util.ElemBuilder()
  protected[this] final val utf8 = Charset.forName("UTF-8")

  /**
   * Read the byte/char at 'i' as a Char.
   *
   * Note that this should not be used on potential multi-byte
   * sequences.
   */
  protected[this] def elem(i: Int): Elem

  /**
   * Read the bytes/chars from 'i' until 'j' as a String.
   */
  protected[this] def sliceString(i: Int, j: Int): CharSequence

  /**
   * Return true iff 'i' is at or beyond the end of the input (EOF).
   */
  protected[this] def atEof(i: Int): Boolean

  /**
   * The reset() method is used to signal that we're working from the
   * given position, and any previous data can be released. Some
   * parsers (e.g.  StringParser) will ignore release, while others
   * (e.g. PathParser) will need to use this information to release
   * and allocate different areas.
   */
  protected[this] def dropBufferUntil(i: Int): Unit

  /**
   * Should be called when parsing is finished.
   */
  protected[this] def close(): Unit

  /**
   * Valid parser states.
   */
  @inline private[this] final val ARRBEG = 6
  @inline private[this] final val OBJBEG = 7
  @inline private[this] final val DATA = 1
  @inline private[this] final val KEY = 2
  @inline private[this] final val COLON = 3
  @inline private[this] final val ARREND = 4
  @inline private[this] final val OBJEND = 5

  protected[this] def newline(i: Int): Unit
  protected[this] def line: Int
  protected[this] def column(i: Int): Int


  /**
    * Parse the JSON document into a single JSON value.
    *
    * The parser considers documents like '333', 'true', and '"foo"' to be
    * valid, as well as more traditional documents like [1,2,3,4,5]. However,
    * multiple top-level objects are not allowed.
    */
  final def parse(facade: Visitor[_, J]): J = {
    val (value, i) = parseTopLevel(0, facade)
    var j = i
    while (!atEof(j)) {
      (elem(j): @switch) match {
        case '\n' => newline(j); j += 1
        case ' ' | '\t' | '\r' => j += 1
        case _ => die(j, "expected whitespace or eof")
      }
    }
    if (!atEof(j)) die(j, "expected eof")
    close()
    value
  }

  /**
   * Used to generate error messages with character info and offsets.
   */
  protected[this] def die(i: Int, msg: String): Nothing = {
    val y = line + 1
    val x = column(i) + 1
    val out = new ujson.util.ElemBuilder()
    ujson.util.RenderUtils.escapeElem(
      out,
      new ArrayCharSequence(Array(elemOps.toInt(elem(i)).toChar)),
      unicode = false
    )
    val s = "%s got %s (line %d, column %d)" format (msg, out.makeString, y, x)
    throw ParseException(s, i, y, x)
  }

  /**
   * Used to generate messages for internal errors.
   *
   * This should only be used in situations where a possible bug in
   * the parser was detected. For errors in user-provided JSON, use
   * die().
   */
  protected[this] def error(msg: String) =
    sys.error(msg)

  /**
   * Parse the given number, and add it to the given context.
   *
   * We don't actually instantiate a number here, but rather pass the
   * string of for future use. Facades can choose to be lazy and just
   * store the string. This ends up being way faster and has the nice
   * side-effect that we know exactly how the user represented the
   * number.
   */
  protected[this] final def parseNum(i: Int, ctxt: ObjArrVisitor[Any, J], facade: Visitor[_, J]): Int = {
    var j = i
    var c = elem(j)
    var decIndex = -1
    var expIndex = -1

    if (c == '-') {
      j += 1
      c = elem(j)
    }
    if (c == '0') {
      j += 1
      c = elem(j)
    } else {
      val j0 = j
      while (elemOps.within('0', c, '9')) { j += 1; c = elem(j) }
      if (j == j0) die(i, "expected digit")
    }

    if (c == '.') {
      decIndex = j - i
      j += 1
      c = elem(j)
      val j0 = j
      while (elemOps.within('0', c, '9')) { j += 1; c = elem(j) }
      if (j0 == j) die(i, "expected digit")
    }

    if (c == 'e' || c == 'E') {
      expIndex = j - i
      j += 1
      c = elem(j)
      if (c == '+' || c == '-') {
        j += 1
        c = elem(j)
      }
      val j0 = j
      while (elemOps.within('0', c, '9')) { j += 1; c = elem(j) }
      if (j0 == j)  die(i, "expected digit")
    }

    ctxt.visitValue(facade.visitFloat64StringParts(sliceString(i, j), decIndex, expIndex, i), i)
    j
  }

  /**
   * Parse the given number, and add it to the given context.
   *
   * This method is a bit slower than parseNum() because it has to be
   * sure it doesn't run off the end of the input.
   *
   * Normally (when operating in rparse in the context of an outer
   * array or object) we don't need to worry about this and can just
   * grab characters, because if we run out of characters that would
   * indicate bad input. This is for cases where the number could
   * possibly be followed by a valid EOF.
   *
   * This method has all the same caveats as the previous method.
   */
  protected[this] final def parseNumSlow(i: Int, facade: Visitor[_, J]): (J, Int) = {
    var j = i
    var c = elem(j)
    var decIndex = -1
    var expIndex = -1

    if (c == '-') {
      // any valid input will require at least one digit after -
      j += 1
      c = elem(j)
    }
    if (c == '0') {
      j += 1
      if (atEof(j)) {
        return (facade.visitFloat64StringParts(sliceString(i, j), decIndex, expIndex, i), j)
      }
      c = elem(j)
    } else {
      val j0 = j
      while (elemOps.within('0', c, '9')) {
        j += 1
        if (atEof(j)) {
          return (facade.visitFloat64StringParts(sliceString(i, j), decIndex, expIndex, i), j)
        }
        c = elem(j)
      }
      if (j0 == j) die(i, "expected digit")
    }

    if (c == '.') {
      // any valid input will require at least one digit after .
      decIndex = j - i
      j += 1
      c = elem(j)
      val j0 = j
      while (elemOps.within('0', c, '9')) {
        j += 1
        if (atEof(j)) {
          return (facade.visitFloat64StringParts(sliceString(i, j), decIndex, expIndex, i), j)
        }
        c = elem(j)
      }
      if(j0 == j) die(i, "expected digit")
    }

    if (c == 'e' || c == 'E') {
      // any valid input will require at least one digit after e, e+, etc
      expIndex = j - i
      j += 1
      c = elem(j)
      if (c == '+' || c == '-') {
        j += 1
        c = elem(j)
      }
      val j0 = j
      while (elemOps.within('0', c, '9')) {
        j += 1
        if (atEof(j)) {

          return (facade.visitFloat64StringParts(sliceString(i, j), decIndex, expIndex, i), j)
        }
        c = elem(j)
      }
      if (j0 == j) die(i, "expected digit")
    }

    (facade.visitFloat64StringParts(sliceString(i, j), decIndex, expIndex, i), j)
  }

  /**
   * Generate a Char from the hex digits of "\u1234" (i.e. "1234").
   *
   * NOTE: This is only capable of generating characters from the basic plane.
   * This is why it can only return Char instead of Int.
   */
  protected[this] final def descape(s: CharSequence): Char = {
    val hc = ujson.util.RenderUtils.hexChars
    var i = 0
    var x = 0
    while (i < 4) {
      x = (x << 4) | hc(s.charAt(i).toInt)
      i += 1
    }
    x.toChar
  }


  /**
   * Parse the JSON constant "true".
   *
   * Note that this method assumes that the first character has already been checked.
   */
  protected[this] final def parseTrue(i: Int, facade: Visitor[_, J]): J =
    if (elem(i + 1) == 'r' && elem(i + 2) == 'u' && elem(i + 3) == 'e') {
      facade.visitTrue(i)
    } else {
      die(i, "expected true")
    }

  /**
   * Parse the JSON constant "false".
   *
   * Note that this method assumes that the first character has already been checked.
   */
  protected[this] final def parseFalse(i: Int, facade: Visitor[_, J]): J =
    if (elem(i + 1) == 'a' && elem(i + 2) == 'l' && elem(i + 3) == 's' && elem(i + 4) == 'e') {
      facade.visitFalse(i)
    } else {
      die(i, "expected false")
    }

  /**
   * Parse the JSON constant "null".
   *
   * Note that this method assumes that the first character has already been checked.
   */
  protected[this] final def parseNull(i: Int, facade: Visitor[_, J]): J =
    if (elem(i + 1) == 'u' && elem(i + 2) == 'l' && elem(i + 3) == 'l') {
      facade.visitNull(i)
    } else {
      die(i, "expected null")
    }

  protected[this] final def parseTopLevel(i: Int, facade: Visitor[_, J]): (J, Int) = {
    try parseTopLevel0(i, facade)
    catch reject(i).orElse[Throwable, Nothing] {
      case e: IndexOutOfBoundsException =>
        throw IncompleteParseException("exhausted input", e)
    }
  }
  /**
   * Parse and return the next JSON value and the position beyond it.
   */
  @tailrec
  protected[this] final def parseTopLevel0(i: Int, facade: Visitor[_, J]): (J, Int) = {
    (elem(i): @switch) match {
      // ignore whitespace
      case ' ' | '\t' | 'r' => parseTopLevel0(i + 1, facade)
      case '\n' => newline(i); parseTopLevel0(i + 1, facade)

      // if we have a recursive top-level structure, we'll delegate the parsing
      // duties to our good friend rparse().
      case '[' => parseNested(ARRBEG, i + 1, facade.visitArray(-1, i), Nil)
      case '{' => parseNested(OBJBEG, i + 1, facade.visitObject(-1, i), Nil)

      // we have a single top-level number
      case '-' | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' =>
        try parseNumSlow(i, facade) catch reject(i)

      // we have a single top-level string
      case '"' =>
        try {
          val (s, j) = parseString(i, false)
          val v = facade.visitString(s, i)
          (v, j)
        } catch reject(i)

      // we have a single top-level constant
      case 't' => (parseTrue(i, facade), i + 4)
      case 'f' => (parseFalse(i, facade), i + 5)
      case 'n' => (parseNull(i, facade), i + 4)

      // invalid
      case _ => die(i, "expected json value")
    }
  }

  def reject(j: Int): PartialFunction[Throwable, Nothing] = {
    case e: Abort =>
      val y = line + 1
      val x = column(j) + 1
      throw new AbortException(e.msg, j, y, x, e)
  }
  /**
   * Tail-recursive parsing method to do the bulk of JSON parsing.
   *
   * This single method manages parser states, data, etc. Except for
   * parsing non-recursive values (like strings, numbers, and
   * constants) all important work happens in this loop (or in methods
   * it calls, like reset()).
   *
   * Currently the code is optimized to make use of switch
   * statements. Future work should consider whether this is better or
   * worse than manually constructed if/else statements or something
   * else. Also, it may be possible to reorder some cases for speed
   * improvements.
   *
   * @param j index/position in the source json
   * @param path the json path in the tree
   */
  @tailrec
  protected[this] final def parseNested(state: Int,
                                        i: Int,
                                        stackHead: ObjArrVisitor[_, J],
                                        stackTail: List[ObjArrVisitor[_, J]]) : (J, Int) = {
    (elem(i): @switch) match{
      case '\n' =>
        newline(i)
        parseNested(state, i + 1, stackHead, stackTail)

      case ' ' | '\t' | '\r' =>
        parseNested(state, i + 1, stackHead, stackTail)

      case '"' =>
        state match{
          case KEY | OBJBEG =>
            val nextJ = try parseObjectKey(i, stackHead) catch reject(i)
            parseNested(COLON, nextJ, stackHead, stackTail)

          case DATA | ARRBEG =>
            val nextJ = try parseStringValue(i, stackHead) catch reject(i)
            parseNested(collectionEndFor(stackHead), nextJ, stackHead, stackTail)

          case _ => dieWithFailureMessage(i, state)
        }

      case ':' =>
        // we are in an object just after a key, expecting to see a colon.
        state match{
          case COLON => parseNested(DATA, i + 1, stackHead, stackTail)
          case _ => dieWithFailureMessage(i, state)
        }

      case '[' =>
        failIfNotData(state, i)
        val ctx =
          try stackHead.subVisitor.asInstanceOf[Visitor[_, J]].visitArray(-1, i)
          catch reject(i)
        parseNested(ARRBEG, i + 1, ctx, stackHead :: stackTail)

      case '{' =>
        failIfNotData(state, i)
        val ctx =
          try stackHead.subVisitor.asInstanceOf[Visitor[_, J]].visitObject(-1, i)
          catch reject(i)
        parseNested(OBJBEG, i + 1, ctx, stackHead :: stackTail)

      case '-' | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' =>
        failIfNotData(state, i)
        val ctx =
          try parseNum(i, stackHead.narrow, stackHead.subVisitor.asInstanceOf[Visitor[_, J]])
          catch reject(i)
        parseNested(collectionEndFor(stackHead), ctx, stackHead, stackTail)

      case 't' =>
        failIfNotData(state, i)
        try stackHead.narrow.visitValue(
          parseTrue(i, stackHead.subVisitor.asInstanceOf[Visitor[_, J]]),
          i
        )
        catch reject(i)
        parseNested(collectionEndFor(stackHead), i + 4, stackHead, stackTail)

      case 'f' =>
        failIfNotData(state, i)
        try stackHead.narrow.visitValue(
          parseFalse(i, stackHead.subVisitor.asInstanceOf[Visitor[_, J]]),
          i
        )
        catch reject(i)
        parseNested(collectionEndFor(stackHead), i + 5, stackHead, stackTail)

      case 'n' =>
        failIfNotData(state, i)
        try stackHead.narrow.visitValue(
          parseNull(i, stackHead.subVisitor.asInstanceOf[Visitor[_, J]]),
          i
        )
        catch reject(i)
        parseNested(collectionEndFor(stackHead), i + 4, stackHead, stackTail)

      case ',' =>
        dropBufferUntil(i)
        (state: @switch) match{
          case ARREND => parseNested(DATA, i + 1, stackHead, stackTail)
          case OBJEND => parseNested(KEY, i + 1, stackHead, stackTail)
          case _ => dieWithFailureMessage(i, state)
        }

      case ']' =>
        (state: @switch) match{
          case ARREND | ARRBEG =>
            tryCloseCollection(stackHead, stackTail, i) match{
              case Some(t) => t
              case None =>
                val stackTailHead = stackTail.head
                parseNested(collectionEndFor(stackTailHead), i + 1, stackTailHead, stackTail.tail)
            }
          case _ => dieWithFailureMessage(i, state)
        }

      case '}' =>
        (state: @switch) match{
          case OBJEND | OBJBEG =>
            tryCloseCollection(stackHead, stackTail, i) match{
              case Some(t) => t
              case None =>
                val stackTailHead = stackTail.head
                parseNested(collectionEndFor(stackTailHead), i + 1, stackTailHead, stackTail.tail)
            }
          case _ => dieWithFailureMessage(i, state)
        }
      case _ => dieWithFailureMessage(i, state)

    }
  }

  private def parseStringValue(i: Int, stackHead: ObjArrVisitor[_, J]) = {
    val (s, nextJ) = parseString(i, false)
    val v = stackHead.subVisitor.visitString(s, i)
    stackHead.narrow.visitValue(v, i)
    nextJ
  }

  private def parseObjectKey(i: Int, stackHead: ObjArrVisitor[_, J]) = {
    val obj = stackHead.asInstanceOf[ObjVisitor[Any, _]]
    val keyVisitor = obj.visitKey(i)
    val (s, nextJ) = parseString(i, true)
    obj.visitKeyValue(keyVisitor.visitString(s, i))
    nextJ
  }

  def dieWithFailureMessage(i: Int, state: Int) = {
    val expected = state match{
      case ARRBEG => "json value or ]"
      case OBJBEG => "json value or }"
      case DATA => "json value"
      case KEY => "json string key"
      case COLON => ":"
      case ARREND => ", or ]"
      case OBJEND => ", or }"
    }
    die(i, s"expected $expected")
  }

  def failIfNotData(state: Int, i: Int) = (state: @switch) match{
    case DATA | ARRBEG => // do nothing
    case _ => dieWithFailureMessage(i, state)
  }

  def tryCloseCollection(stackHead: ObjArrVisitor[_, J], stackTail: List[ObjArrVisitor[_, J]], i: Int) = {
    if (stackTail.isEmpty) {
      Some(try stackHead.visitEnd(i) catch reject(i), i + 1)
    } else {
      val ctxt2 = stackTail.head.narrow
      try ctxt2.visitValue(stackHead.visitEnd(i), i) catch reject(i)
      None

    }
  }
  def collectionEndFor(stackHead: ObjArrVisitor[_, _]) = {
    if (stackHead.isObj) OBJEND
    else ARREND
  }

  /**
    * See if the string has any escape sequences. If not, return the
    * end of the string. If so, bail out and return -1.
    *
    * This method expects the data to be in UTF-16 and accesses it as
    * chars.
    */
  protected[this] final def parseStringSimple(i: Int): Int = {
    var j = i
    var c = elemOps.toUnsignedInt(elem(j))
    while (c != '"') {
      if (c < ' ') die(j, s"control char (${c}) in string")
      if (c == '\\') return -1 - j
      j += 1
      outputBuilder.append(c)
      c = elemOps.toUnsignedInt(elem(j))
    }
    j + 1
  }

  /**
    * Parse a string that is known to have escape sequences.
    */
  protected[this] final def parseStringComplex(i0: Int): (CharSequence, Int) = {

    var i = i0
    var c = elemOps.toUnsignedInt(elem(i))
    while (c != '"') {
      if (c < ' ') {
        die(i, s"control char (${c}) in string")
      } else if (c == '\\') {
        (elem(i + 1): @switch) match {
          case 'b' => { outputBuilder.append('\b'); i += 2 }
          case 'f' => { outputBuilder.append('\f'); i += 2 }
          case 'n' => { outputBuilder.append('\n'); i += 2 }
          case 'r' => { outputBuilder.append('\r'); i += 2 }
          case 't' => { outputBuilder.append('\t'); i += 2 }

          case '"' => { outputBuilder.append('"'); i += 2 }
          case '/' => { outputBuilder.append('/'); i += 2 }
          case '\\' => { outputBuilder.append('\\'); i += 2 }

          // if there's a problem then descape will explode
          case 'u' => { outputBuilder.append(descape(sliceString(i + 2, i + 6))); i += 6 }

          case c => die(i + 1, s"illegal escape sequence after \\")
        }
      } else {
        // this case is for "normal" code points that are just one Char.
        //
        // we don't have to worry about surrogate pairs, since those
        // will all be in the ranges D800–DBFF (high surrogates) or
        // DC00–DFFF (low surrogates).
        outputBuilder.append(c)
        i += 1
      }
      c = elemOps.toUnsignedInt(elem(i))
    }

    (outputBuilder.makeString, i + 1)
  }

  /**
    * Parse the string according to JSON rules, and add to the given
    * context.
    *
    * This method expects the data to be in UTF-16, and access it as
    * Char. It performs the correct checks to make sure that we don't
    * interpret a multi-char code point incorrectly.
    */
  protected[this] final def parseString(i: Int,  key: Boolean): (CharSequence, Int) = {
    outputBuilder.reset()
    val k = parseStringSimple(i + 1)
    if (k >= 0) (outputBuilder.makeString, k)
    else parseStringComplex(-k - 1)
  }
}
