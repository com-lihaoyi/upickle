package upickle.core

class UTestFramework extends utest.runner.Framework {
  override def exceptionStackFrameHighlighter(s: StackTraceElement) = {
    s.getClassName.startsWith("upickle.") ||
    s.getClassName.startsWith("upack.") ||
    s.getClassName.startsWith("ujson.")
  }
}
