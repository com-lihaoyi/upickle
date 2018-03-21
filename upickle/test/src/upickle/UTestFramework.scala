package upickle

class UTestFramework extends utest.runner.MillFramework {
  override def exceptionStackFrameHighlighter(s: StackTraceElement) = {
    s.getClassName.startsWith("upickle.")
  }
}
