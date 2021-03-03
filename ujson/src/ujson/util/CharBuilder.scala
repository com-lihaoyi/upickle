package ujson.util

/**
 * CharBuilder is a specialized way to build Strings.
 *
 * It wraps a (growable) array of characters, and can transform
 * additional String or Char data to be added to its buffer.
 */
private[ujson] final class CharBuilder {
  @inline final def INITIALSIZE = 32

  private[this] var cs = new Array[Char](INITIALSIZE)
  private[this] var len = 0

  def reset(): Unit = {
    len = 0
  }

  def makeString: String = new String(cs, 0, len)


  def append(c: Char): Unit = {
    if (len == cs.length) cs = java.util.Arrays.copyOf(cs, cs.length * 2)
    cs(len) = c
    len = len + 1
  }
}
