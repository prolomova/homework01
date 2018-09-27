package fintech.homework01

import scala.collection.mutable
import util.control.Breaks._

// Используя функции io.readLine и io.printLine напишите игру "Виселица"
// Пример ввода и тест можно найти в файле src/test/scala/fintech/homework01/HangmanTest.scala
// Тест можно запустить через в IDE или через sbt (написав в консоли sbt test)

// Правила игры "Виселица"
// 1) Загадывается слово
// 2) Игрок угадывает букву
// 3) Если такая буква есть в слове - они открывается
// 4) Если нет - рисуется следующий элемент висельника
// 5) Последней рисуется "веревка". Это означает что игрок проиграл
// 6) Если игрок все еще жив - перейти к пункту 2

// Пример игры:

// Word: _____
// Guess a letter:
// a
// Word: __a_a
// Guess a letter:
// b
// +----
// |
// |
// |
// |
// |

// и т.д.


class Hangman(io: IODevice) {

  val maxMistakesNumber = 8

  def isCorrect(input: String): Boolean = {
    input.length == 1 && input.charAt(0).isLetter
  }

  def findIndexes(word: String, letter: Char): Set[AnyVal] = {
    Stream.tabulate(word.length)(i => if (word.charAt(i) == letter) i).toSet
  }

  def replaceLetters(guessedLetters: String, letter : Char, indexes: Set[AnyVal]): String = {
    Stream.tabulate(guessedLetters.length)(i => if (indexes.contains(i)) letter else guessedLetters.charAt(i)).mkString
  }

  def play(word: String): Unit = {
    var mistakesCount = 0
    var guessedLetters = "_" * word.length()
    val previousLetters = new mutable.HashSet[Char]()
    while (mistakesCount < maxMistakesNumber && word != guessedLetters)
    {
      breakable {
        io.printLine("Word: " + guessedLetters + "\nGuess a letter:")
        val input = io.readLine()
        if (!isCorrect(input)) break
        val letter = input.charAt(0)
        if (previousLetters.contains(letter)) break
        if (word.contains(letter))
          guessedLetters = replaceLetters(guessedLetters, letter, findIndexes(word, letter))
        else mistakesCount += 1
        previousLetters.add(letter)
        if (mistakesCount > 0) io.printLine(stages(mistakesCount - 1))
      }
    }
    if (mistakesCount == maxMistakesNumber) io.printLine("You are dead")
    else io.printLine("Win")
  }
  val stages = List(
    """+----
      ||
      ||
      ||
      ||
      ||
      |""".stripMargin,
    """+----
      ||
      ||   O
      ||
      ||
      ||
      |""".stripMargin,
    """+----
      ||
      ||   O
      ||   |
      ||
      ||
      |""".stripMargin,
    """+----
      ||
      ||   O
      ||   |
      ||  /
      ||
      |""".stripMargin,
    """+----
      ||
      ||   O
      ||   |
      ||  / \
      ||
      |""".stripMargin,
    """+----
      ||
      ||   O
      ||  /|
      ||  / \
      ||
      |""".stripMargin,
    """+----
      ||
      ||   O
      ||  /|\
      ||  / \
      ||
      |""".stripMargin,
    """+----
      ||   |
      ||   O
      ||  /|\
      ||  / \
      ||
      |""".stripMargin
  )
}

trait IODevice {
  def printLine(text: String): Unit
  def readLine(): String
}