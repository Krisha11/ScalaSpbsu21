package org.spbsu.mkn.scala

import scala.io.StdIn.readLine
import scala.util.Random
import scala.collection.mutable

object TheGame {

  sealed trait GuessResult
  case class Correct(numTries: Int) extends GuessResult
  case class Incorrect(bulls: Int, cows: Int) extends GuessResult

  class RepeatingDigitsException extends RuntimeException
  class WrongNumberLengthException(expected: Int, got: Int) extends RuntimeException

  def generateNumberString(length: Int): String = {
    val cur : mutable.HashSet[Int] = mutable.HashSet.empty

    for (i <- 1 to length) {
      var t = Random.between(0, 10)
      while(cur.contains(t)) {
        t = Random.between(0, 10)
      }
      cur.add(t)
    }
    val res : mutable.StringBuilder = new mutable.StringBuilder()
    for (i <- Random.shuffle(cur.toSet.toList)) {
      res.append(i.toString())
    }

    return res.toString()
  }

  def validate(secret: String, userInput: String, numTries: Int = 1): GuessResult = {
    if (secret.length != userInput.length)
      throw new WrongNumberLengthException(secret.length, userInput.length)

    if (secret.toSet.size != secret.length)
      throw new RepeatingDigitsException()

    if (secret == userInput)
      return Correct(numTries)

    var a = 0
    var b = 0
    for (i <- 0 to secret.length - 1; if secret.charAt(i) == userInput.charAt(i))
      a = a + 1

    for (i <- '0' to '9'; if secret.toSet.contains(i) && userInput.contains(i))
      b = b + 1

    Incorrect(a, b - a)
  }

  def main(args: Array[String]): Unit = {
    print("Enter your name: ")
    val name = readLine()
    println(s"Hello, $name!")

    print("Enter the word length: ")
    val len = readLine().toInt
    val secret = generateNumberString(len)
    var numTries = 1

    var flag = false
    while(!flag) {
      println("Your turn!")
      var s = readLine()
      val res = validate(secret, s, numTries)
      if (res.isInstanceOf[TheGame.Correct]) {
        println("Win!!!")
        println(res)
        flag = true
      }
      else if (res.isInstanceOf[TheGame.Incorrect])
        println(res)
    }

  }
}
