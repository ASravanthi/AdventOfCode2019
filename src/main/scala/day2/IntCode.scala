package day2

import scala.collection.mutable.ListBuffer
import scala.io.Source
import util.control.Breaks._

object IntCode {

  def main(args: Array[String]): Unit = {
    val input = Source.fromResource("day2/input.txt").getLines().toList(0).split(",").map(_.toInt)

    //part1
    val output = calculateOutput(input.to[ListBuffer], 12, 2)
    println(output) //12490719

    //part2
    breakable {
      for (noun <- 0 to 99) {
        for (verb <- 0 to 99) {
          val output = calculateOutput(input.to[ListBuffer], noun, verb)
          if (output == 19690720) {
            println(100 * noun + verb) //2003
            break
          }
        }
      }
    }
  }

  def calculateOutput(input: ListBuffer[Int], noun: Int, verb: Int) = {
    input(1) = noun
    input(2) = verb

    breakable {
      for (index <- input.indices; if (index % 4) == 0 && index + 3 < input.size) {
        val opcode = input(index)

        val pos1 = input(index + 1)
        val value1 = input(pos1)

        val pos2 = input(index + 2)
        val value2 = input(pos2)

        val outputPos = input(index + 3)

        opcode match {
          case 1 => input(outputPos) = value1 + value2
          case 2 => input(outputPos) = value1 * value2
          case 99 => break
        }
      }
    }
    input(0)
  }
}
