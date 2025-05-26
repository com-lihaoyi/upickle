package ujson

import java.io.StringWriter

import upickle.core.{Abort, AbortException, ObjArrVisitor, ObjVisitor, Visitor}
import java.nio.CharBuffer
import java.nio.charset.Charset

import scala.annotation.{switch, tailrec}

/**
  * A specialized JSON parse that can parse Elems (Chars or Bytes), sending
  * method calls to the given [[upickle.core.Visitor]].
  *
  * Generally has a lot of tricks for performance: e.g. having duplicate
  * implementations for nested v.s. top-level parsing, using an `ElemBuilder`
  * to construct the `CharSequences` that `visitString` requires, etc.
  */
abstract class ElemParser[J] extends upickle.core.BufferingElemParser{
  private[this] val elemOps = upickle.core.ElemOps
  private[this] val outputBuilder = new upickle.core.ElemBuilder()

  /**
   * A fast-path to check whether an index can be safely accessed, before calling
   * [[getElemUnsafe]]. Together, it is similar to calling [[getElemSafe]], except
   * this returns the new safeIndex which the caller can then use to call
   * [[getElemUnsafe]] multiple times before needing to call this again.
   *
   */
  def requestUntilOrThrow(j: Int): Unit = checkSafeIndex(j)

  def checkSafeIndex(j: Int): Int = {
    val newSafeIndex = requestUntilGetSafeIndex(j)
    if (newSafeIndex == j) throw new IncompleteParseException("exhausted input")
    newSafeIndex
  }

  override def getElemSafe(i: Int): Elem = {
    requestUntilOrThrow(i)
    getElemUnsafe(i)
  }

  /**
   * Return true iff 'i' is at or beyond the end of the input (EOF).
   */
  protected[this] def atEof(i: Int) = requestUntil(i)

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
      (getElemSafe(j): @switch) match {
        case '\n' | ' ' | '\t' | '\r' => j += 1
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
    val out = new upickle.core.ElemBuilder()
    upickle.core.RenderUtils.escapeElem(
      new upickle.core.CharBuilder(),
      out,
      CharBuffer.wrap(Array(elemOps.toInt(getElemSafe(i)).toChar)),
      escapeUnicode = false,
      true
    )
    val s = "%s got %s" format (msg, out.makeString())
    throw ujson.ParseException(s, i)
  }


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
    var c = getElemSafe(j)
    var decIndex = -1
    var expIndex = -1

    if (c == '-') {
      j += 1
      c = getElemSafe(j)
    }
    if (c == '0') {
      j += 1
      c = getElemSafe(j)
    } else {
      val j0 = j
      while (elemOps.within('0', c, '9')) {
        j += 1;
        c = getElemSafe(j)
      }
      if (j == j0) die(i, "expected digit")
    }

    if (c == '.') {
      decIndex = j - i
      j += 1
      c = getElemSafe(j)
      val j0 = j
      while (elemOps.within('0', c, '9')) {
        j += 1
        c = getElemSafe(j)
      }
      if (j0 == j) die(i, "expected digit")
    }

    if (c == 'e' || c == 'E') {
      expIndex = j - i
      j += 1
      c = getElemSafe(j)
      if (c == '+' || c == '-') {
        j += 1
        c = getElemSafe(j)
      }
      val j0 = j
      while (elemOps.within('0', c, '9')) {
        j += 1
        c = getElemSafe(j)
      }
      if (j0 == j)  die(i, "expected digit")
    }

    ctxt.visitValue(visitFloat64StringPartsWithWrapper(facade, decIndex, expIndex, i, j), i)
    j
  }

  def visitFloat64StringPartsWithWrapper(facade: Visitor[_, J],
                                         decIndex: Int,
                                         expIndex: Int,
                                         i: Int,
                                         j: Int) = {
    facade.visitFloat64ElemParts(
      getBuffer,
      i - getFirstIdx,
      j - i,
      decIndex,
      expIndex,
      i
    )
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
  protected[this] final def parseNumTopLevel(i: Int, facade: Visitor[_, J]): (J, Int) = {
    var j = i
    var c = getElemSafe(j)
    var decIndex = -1
    var expIndex = -1

    if (c == '-') {
      // any valid input will require at least one digit after -
      j += 1
      c = getElemSafe(j)
    }
    if (c == '0') {
      j += 1
      if (atEof(j)) {
        return (visitFloat64StringPartsWithWrapper(facade, decIndex, expIndex, i, j), j)
      }
      c = getElemSafe(j)
    } else {
      val j0 = j
      while (elemOps.within('0', c, '9')) {
        j += 1
        if (atEof(j)) {
          return (visitFloat64StringPartsWithWrapper(facade, decIndex, expIndex, i, j), j)
        }
        c = getElemSafe(j)
      }
      if (j0 == j) die(i, "expected digit")
    }

    if (c == '.') {
      // any valid input will require at least one digit after .
      decIndex = j - i
      j += 1
      c = getElemSafe(j)
      val j0 = j
      while (elemOps.within('0', c, '9')) {
        j += 1
        if (atEof(j)) {
          return (visitFloat64StringPartsWithWrapper(facade, decIndex, expIndex, i, j), j)
        }
        c = getElemSafe(j)
      }
      if(j0 == j) die(i, "expected digit")
    }

    if (c == 'e' || c == 'E') {
      // any valid input will require at least one digit after e, e+, etc
      expIndex = j - i
      j += 1
      c = getElemSafe(j)
      if (c == '+' || c == '-') {
        j += 1
        c = getElemSafe(j)
      }
      val j0 = j
      while (elemOps.within('0', c, '9')) {
        j += 1
        if (atEof(j)) {
          return (visitFloat64StringPartsWithWrapper(facade, decIndex, expIndex, i, j), j)
        }
        c = getElemSafe(j)
      }
      if (j0 == j) die(i, "expected digit")
    }

    (visitFloat64StringPartsWithWrapper(facade, decIndex, expIndex, i, j), j)
  }

  /**
   * Generate a Char from the hex digits of "\u1234" (i.e. "1234").
   *
   * NOTE: This is only capable of generating characters from the basic plane.
   * This is why it can only return Char instead of Int.
   */
  protected[this] final def descape(i: Int): Char = {
    import upickle.core.RenderUtils.hex
    var x = 0
    x = (x << 4) | hex(getElemSafe(i+2).toInt)
    x = (x << 4) | hex(getElemSafe(i+3).toInt)
    x = (x << 4) | hex(getElemSafe(i+4).toInt)
    x = (x << 4) | hex(getElemSafe(i+5).toInt)
    x.toChar
  }


  /**
   * Parse the JSON constant "true".
   *
   * Note that this method assumes that the first character has already been checked.
   */
  protected[this] final def parseTrue(i: Int, facade: Visitor[_, J]): J = {
    requestUntilOrThrow(i + 3)
    if (getElemUnsafe(i + 1) == 'r' && getElemUnsafe(i + 2) == 'u' && getElemUnsafe(i + 3) == 'e') {
      facade.visitTrue(i)
    } else {
      die(i, "expected true")
    }
  }

  /**
   * Parse the JSON constant "false".
   *
   * Note that this method assumes that the first character has already been checked.
   */
  protected[this] final def parseFalse(i: Int, facade: Visitor[_, J]): J = {
    requestUntilOrThrow(i + 4)

    if (getElemUnsafe(i + 1) == 'a' && getElemUnsafe(i + 2) == 'l' && getElemUnsafe(i + 3) == 's' && getElemUnsafe(i + 4) == 'e') {
      facade.visitFalse(i)
    } else {
      die(i, "expected false")
    }
  }

  /**
   * Parse the JSON constant "null".
   *
   * Note that this method assumes that the first character has already been checked.
   */
  protected[this] final def parseNull(i: Int, facade: Visitor[_, J]): J = {
    requestUntilOrThrow(i + 3)
    if (getElemUnsafe(i + 1) == 'u' && getElemUnsafe(i + 2) == 'l' && getElemUnsafe(i + 3) == 'l') {
      facade.visitNull(i)
    } else {
      die(i, "expected null")
    }
  }

  protected[this] final def parseTopLevel(i: Int, facade: Visitor[_, J]): (J, Int) = {
    try parseTopLevel0(i, facade)
    catch reject(i)
  }
  /**
   * Parse and return the next JSON value and the position beyond it.
   */
  @tailrec
  protected[this] final def parseTopLevel0(i: Int, facade: Visitor[_, J]): (J, Int) = {
    (getElemSafe(i): @switch) match {
      // ignore whitespace
      case ' ' | '\t' | '\r' => parseTopLevel0(i + 1, facade)
      case '\n' => parseTopLevel0(i + 1, facade)

      // if we have a recursive top-level structure, we'll delegate the parsing
      // duties to our good friend rparse().
      case '[' => parseNested(ARRBEG, i + 1, facade.visitArray(-1, i), Nil)
      case '{' => parseNested(OBJBEG, i + 1, facade.visitObject(-1, true, i), Nil)

      // we have a single top-level number
      case '-' | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' => parseNumTopLevel(i, facade)

      // we have a single top-level string
      case '"' => parseStringTopLevel(i, facade)

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
      throw new AbortException(e.msg, j, -1, -1, e)
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
    (getElemSafe(i): @switch) match{
      case ' ' | '\t' | '\r' | '\n' =>
        parseNested(state, i + 1, stackHead, stackTail)

      case '"' =>
        state match{
          case KEY | OBJBEG =>
            val nextJ = try parseStringKey(i, stackHead) catch reject(i)
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
          try stackHead.subVisitor.asInstanceOf[Visitor[_, J]].visitObject(-1, true, i)
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
  protected[this] final def parseStringSimple(i: Int): Int = parseStringSimple(i, 0)
  protected[this] final def parseStringSimple(i: Int, safeIndex0: Int): Int = {
    var j = i
    var safeIndex = safeIndex0
    while (true) {
      // request batches of `Elem` at a time, so within each batch we can use
      // `getElemUnsafe` to avoid making a `requestUntil` for every single
      // element we fetch
      if (j >= safeIndex) safeIndex = checkSafeIndex(j)
      val c = getElemUnsafe(j)
      (c: @switch) match {
        case '"' => return j + 1
        case 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 |
             10 | 11 | 12 | 13 | 14 | 15 | 16 | 17 | 18 | 19 |
             20 | 21 | 22 | 23 | 24 | 25 | 26 | 27 | 28 | 29 |
             30 | 31 => die(j, s"control char (${upickle.core.ElemOps.toUnsignedInt(c)}) in string")
        case '\\' => return -1 - j
        case _ => j += 1
      }
    }
    ???
  }

  /**
    * Parse a string that is known to have escape sequences.
    */
  protected[this] final def parseStringComplex(i0: Int): Int = parseStringComplex(i0, 0)
  protected[this] final def parseStringComplex(i0: Int, safeIndex0: Int): Int = {
    val elemOps = upickle.core.ElemOps
    var i = i0
    var safeIndex = safeIndex0
    while (true) {
      val c = elemOps.toUnsignedInt(getElemSafe(i))
      if (c == '"') return i + 1
      else if (c < ' ') die(i, s"control char (${c}) in string")
      else if (c == '\\') {
        if (i + 1 >= safeIndex) safeIndex = checkSafeIndex(i + 1)
        (getElemUnsafe(i + 1): @switch) match {
          case 'b' => { outputBuilder.append('\b'); i += 2 }
          case 'f' => { outputBuilder.append('\f'); i += 2 }
          case 'n' => { outputBuilder.append('\n'); i += 2 }
          case 'r' => { outputBuilder.append('\r'); i += 2 }
          case 't' => { outputBuilder.append('\t'); i += 2 }
          case '"' => { outputBuilder.append('"'); i += 2 }
          case '/' => { outputBuilder.append('/'); i += 2 }
          case '\\' => { outputBuilder.append('\\'); i += 2 }

          // if there's a problem then descape will explode
          case 'u' =>
            outputBuilder.appendC(descape(i))
            i += 6

          case c => die(i + 1, s"illegal escape sequence after \\")
        }
      }else{
        // this case is for "normal" code points that are just one Char.
        //
        // we don't have to worry about surrogate pairs, since those
        // will all be in the ranges D800–DBFF (high surrogates) or
        // DC00–DFFF (low surrogates).
        val k = parseStringSimple(i, safeIndex)
        val normalizedK = if (k >= 0) k else -k
        appendElemsToBuilder(outputBuilder, i, normalizedK - i - 1)
        i = normalizedK - 1
      }
    }
    ???
  }

  /**
    * Parse the string according to JSON rules, and add to the given
    * context.
    *
    * This method expects the data to be in UTF-16, and access it as
    * Char. It performs the correct checks to make sure that we don't
    * interpret a multi-char code point incorrectly.
    */
  protected[this] final def parseStringValue(i: Int, stackHead: ObjArrVisitor[_, J]): Int = {

    val k = parseStringSimple(i + 1)
    if (k >= 0) {
      visitString(i, unsafeCharSeqForRange(i + 1, k - i - 2), stackHead)
      k
    } else {
      val k2 = parseStringToOutputBuilder(i, k)
      visitString(i, outputBuilder.makeString(), stackHead)
      k2
    }
  }

  protected[this] final def parseStringKey(i: Int, stackHead: ObjArrVisitor[_, J]): Int = {

    val k = parseStringSimple(i + 1)
    if (k >= 0) {
      visitStringKey(i, unsafeCharSeqForRange(i + 1, k - i - 2), stackHead)
      k
    } else {
      val k2 = parseStringToOutputBuilder(i, k)
      visitStringKey(i, outputBuilder.makeString(), stackHead)
      k2
    }
  }


  def parseStringToOutputBuilder(i: Int, k: Int) = {
    outputBuilder.reset()
    appendElemsToBuilder(outputBuilder, i + 1, -k - 2 - i)
    val k2 = parseStringComplex(-k - 1)
    k2
  }

  def visitString(i: Int, s: CharSequence, stackHead: ObjArrVisitor[_, J]) = {
    val v = stackHead.subVisitor.visitString(s, i)
    stackHead.narrow.visitValue(v, i)
  }
  def visitStringKey(i: Int, s: CharSequence, stackHead: ObjArrVisitor[_, J]) = {
    val obj = stackHead.asInstanceOf[ObjVisitor[Any, _]]
    val keyVisitor = obj.visitKey(i)
    obj.visitKeyValue(keyVisitor.visitString(s, i))
  }


  protected[this] final def parseStringTopLevel(i: Int, facade: Visitor[_, J]): (J, Int) = {

    val k = parseStringSimple(i + 1)
    if (k >= 0) {
      val res = facade.visitString(unsafeCharSeqForRange(i + 1, k - i - 2), i)
      (res, k)
    } else {
      val k2 = parseStringToOutputBuilder(i, k)
      val res = facade.visitString(outputBuilder.makeString(), i)
      (res, k2)
    }
  }
}
