package fintech.homework01

class TestIO(base: String) extends IODevice {
  var queue: List[String] = base.lines.toList.tail // skip first line

  def readLine(): String = {
    if (isExhausted)
      throw new IllegalStateException("Can't read from exhausted test io")

    val expected = queue.head
    if (!expected.startsWith("> "))
      throw new IllegalStateException(s"Read operation out of order, expected print out of '$expected'")

    queue = queue.tail
    expected.substring(2)
  }

  def printLine(text: String): Unit = {
    for (line <- text.lines) {
      if (isExhausted)
        throw new IllegalStateException("Can't print to exhausted test io")

      val expected = queue.head
      if (expected.startsWith("> "))
        throw new IllegalStateException(s"Print operation out of order, expected read of '$expected'")

      if (expected != line)
        throw new IllegalStateException(s"Unexpected print out, should be '$expected'")

      queue = queue.tail
    }
  }

  def isExhausted: Boolean = queue.isEmpty

  override def toString: String = "TestIO(queue: " + queue.toString + ")"
}
