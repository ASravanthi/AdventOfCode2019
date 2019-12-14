package day4

import scala.collection.mutable
import scala.io.Source

object SecureContainer {

  def main(args: Array[String]): Unit = {
    val input = Source.fromResource("day4/input.txt").getLines().toList(0).split("-").map(_.toInt)
    val from = input(0)
    val to = input(1)

    println(findNoOfMatchingPasswords(from, to))
  }

  private def findNoOfMatchingPasswords(from: Int, to: Int): Int = {
    List.range(from, to + 1).filter(password => {
      password >= 100000 &&
        password <= 999999 &&
        hasConsecutiveDigitsSame(password) &&
        hasIncreasingSequence(password) && //till here part 1 //481
        notPartOfLargerGroupOfMatchingDigits(password) // part 2 //299
    }).size
  }

  private def notPartOfLargerGroupOfMatchingDigits(number: Int): Boolean = {
    val digits = number.toString.toCharArray
    val digitCountMapping = mutable.Map.empty[Char, Int]
    for (i <- digits.indices; if i + 1 < digits.size) {
      val currentDigit = digits(i)
      val nextDigit = digits(i + 1)
      if (currentDigit == nextDigit) {
        if (digitCountMapping.contains(currentDigit)) {
          digitCountMapping(currentDigit) = digitCountMapping(currentDigit) + 1
        } else {
          digitCountMapping(currentDigit) = 2
        }
      }
    }
    digitCountMapping.values.toList.contains(2)
  }

  private def hasConsecutiveDigitsSame(number: Int): Boolean = {
    val digits = number.toString.toCharArray
    for (i <- digits.indices; if i + 1 < digits.size) {
      if (digits(i) == digits(i + 1)) {
        return true
      }
    }
    false
  }

  private def hasIncreasingSequence(number: Int): Boolean = {
    val digits = number.toString.toCharArray
    for (i <- digits.indices; if i + 1 < digits.size) {
      if (digits(i) > digits(i + 1)) {
        return false
      }
    }
    true
  }
}
