package fintech.homework01

import org.scalatest._

class HangmanTest extends FlatSpec with Matchers {

  "Hangman" should "be a playable game" in {
    val testIO = new TestIO(testData)
    val game = new Hangman(testIO)

    game.play("scala")

    testIO.queue should be(Nil)
  }

  // "> " помечают ввод от пользователя
  val testData =
    """
Word: _____
Guess a letter:
> a
Word: __a_a
Guess a letter:
> b
+----
|
|
|
|
|
Word: __a_a
Guess a letter:
> c
+----
|
|
|
|
|
Word: _ca_a
Guess a letter:
> d
+----
|
|   O
|
|
|
Word: _ca_a
Guess a letter:
> e
+----
|
|   O
|   |
|
|
Word: _ca_a
Guess a letter:
> f
+----
|
|   O
|   |
|  /
|
Word: _ca_a
Guess a letter:
> g
+----
|
|   O
|   |
|  / \
|
Word: _ca_a
Guess a letter:
> h
+----
|
|   O
|  /|
|  / \
|
Word: _ca_a
Guess a letter:
> i
+----
|
|   O
|  /|\
|  / \
|
Word: _ca_a
Guess a letter:
> j
+----
|   |
|   O
|  /|\
|  / \
|
You are dead"""
}