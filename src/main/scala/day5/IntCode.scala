package day5

import scala.collection.mutable.ListBuffer
import scala.io.Source
import scala.util.control.Breaks.{break, breakable}

object IntCode {

  def main(args: Array[String]): Unit = {
    val pInput = Source.fromResource("day5/input.txt").getLines().toList(0).split(",").map(_.toInt)
    //    val input = 1 //part1
    val input = 5 //part2

    printDiagnosticCode(pInput, input)
  }

  def printDiagnosticCode(pInput: Array[Int], input: Int) = {
    val mInput = pInput.to[ListBuffer]

    breakable {
      var ptr = 0
      while (ptr < mInput.size) {
        val instruction = "%05d".format(mInput(ptr))

        val opcode = instruction.substring(3, 5).toInt
        val mode1 = instruction.substring(2, 3).toInt
        val mode2 = instruction.substring(1, 2).toInt
        //        val mode3 = instruction.substring(0, 1).toInt

        opcode match {
          case 1 => {
            val (value1, value2) = getValues(mInput, ptr, mode1, mode2)
            mInput(mInput(ptr + 3)) = value1 + value2
            ptr += 4
          }
          case 2 => {
            val (value1, value2) = getValues(mInput, ptr, mode1, mode2)
            mInput(mInput(ptr + 3)) = value1 * value2
            ptr += 4
          }
          case 99 => break
          case 3 => {
            mInput(mInput(ptr + 1)) = input
            ptr += 2
          }
          case 4 => {
            println(mInput(mInput(ptr + 1))) //5182797 //12077198
            ptr += 2
          }
          case 5 =>
            val (value1, value2) = getValues(mInput, ptr, mode1, mode2)
            if (value1 != 0) {
              ptr = value2
            } else {
              ptr += 3
            }
          case 6 =>
            val (value1, value2) = getValues(mInput, ptr, mode1, mode2)
            if (value1 == 0) {
              ptr = value2
            } else {
              ptr += 3
            }
          case 7 =>
            val (value1, value2) = getValues(mInput, ptr, mode1, mode2)
            if (value1 < value2) {
              mInput(mInput(ptr + 3)) = 1
            } else {
              mInput(mInput(ptr + 3)) = 0
            }
            ptr += 4
          case 8 =>
            val (value1, value2) = getValues(mInput, ptr, mode1, mode2)
            if (value1 == value2) {
              mInput(mInput(ptr + 3)) = 1
            } else {
              mInput(mInput(ptr + 3)) = 0
            }
            ptr += 4
        }
      }
    }
  }

  private def getValues(mInput: ListBuffer[Int], ptr: Int, mode1: Int, mode2: Int) = {
    val value1 = mode1 match {
      case 0 => mInput(mInput(ptr + 1))
      case 1 => mInput(ptr + 1)
    }
    val value2 = mode2 match {
      case 0 => mInput(mInput(ptr + 2))
      case 1 => mInput(ptr + 2)
    }
    (value1, value2)
  }
}
