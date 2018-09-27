package fintech.homework01

import scala.io.StdIn

object HangmanApp {
  def main(args: Array[String]): Unit = {

    val console = new IODevice {
      override def printLine(text: String): Unit = println(text)
      override def readLine(): String = StdIn.readLine()
    }

    new Hangman(console).play("scala")
  }
}
