package day9

import scala.collection.mutable
import scala.io.Source
import scala.util.control.Breaks.{break, breakable}

object IntCode {

  def main(args: Array[String]): Unit = {
    val inputSequence = Source.fromResource("day9/input.txt").getLines().toList(0).split(",").map(_.toLong)
    val input = 1 //part1 o/p 4080871669 input = 2 //part2 o/p 75202
    val pInput = mutable.Map[Long, Long]()
    (0 until inputSequence.size).foreach(i => pInput(i) = inputSequence(i))

    printDiagnosticCode(pInput, input)
  }

  def printDiagnosticCode(mInput: mutable.Map[Long, Long], input: Long) = {
    breakable {
      var ptr: Long = 0
      var relativeBase: Long = 0L
      while (ptr < mInput.size) {
        val instruction = "%05d".format(mInput(ptr))

        val opcode = instruction.substring(3, 5).toInt
        val mode1 = instruction.substring(2, 3).toInt
        val mode2 = instruction.substring(1, 2).toInt
        val mode3 = instruction.substring(0, 1).toInt

        opcode match {
          case 1 => {
            val (value1, value2, address3) = getValues(mInput, ptr, mode1, mode2, mode3, relativeBase)
            mInput(address3) = value1 + value2
            ptr += 4
          }
          case 2 => {
            val (value1, value2, address3) = getValues(mInput, ptr, mode1, mode2, mode3, relativeBase)
            mInput(address3) = value1 * value2
            ptr += 4
          }
          case 99 => break
          case 3 => {
            val address = getAddress(mInput, ptr, mode1, relativeBase)
            mInput(address) = input
            ptr += 2
          }
          case 4 => {
            val address = getAddress(mInput, ptr, mode1, relativeBase)
            println(mInput(address))
            ptr += 2
          }
          case 5 =>
            val (value1, value2, _) = getValues(mInput, ptr, mode1, mode2, mode3, relativeBase)
            if (value1 != 0) {
              ptr = value2
            } else {
              ptr += 3
            }
          case 6 =>
            val (value1, value2, _) = getValues(mInput, ptr, mode1, mode2, mode3, relativeBase)
            if (value1 == 0) {
              ptr = value2
            } else {
              ptr += 3
            }
          case 7 =>
            val (value1, value2, address3) = getValues(mInput, ptr, mode1, mode2, mode3, relativeBase)
            if (value1 < value2) {
              mInput(address3) = 1
            } else {
              mInput(address3) = 0
            }
            ptr += 4
          case 8 =>
            val (value1, value2, address3) = getValues(mInput, ptr, mode1, mode2, mode3, relativeBase)
            if (value1 == value2) {
              mInput(address3) = 1
            } else {
              mInput(address3) = 0
            }
            ptr += 4
          case 9 =>
            val address = getAddress(mInput, ptr, mode1, relativeBase)
            relativeBase = relativeBase + mInput(address)
            ptr += 2
        }
      }
    }
  }

  private def getValues(mInput: mutable.Map[Long, Long], ptr: Long, mode1: Int, mode2: Int, mode3: Int, relativeBase: Long): (Long, Long, Long) = {
    val value1 = mode1 match {
      case 0 => mInput.getOrElse(mInput(ptr + 1), 0L)
      case 1 => mInput.getOrElse(ptr + 1, 0L)
      case 2 => mInput.getOrElse(mInput(ptr + 1) + relativeBase, 0L)
    }
    val value2 = mode2 match {
      case 0 => mInput.getOrElse(mInput(ptr + 2), 0L)
      case 1 => mInput.getOrElse(ptr + 2, 0L)
      case 2 => mInput.getOrElse(mInput(ptr + 2) + relativeBase, 0L)
    }
    val address3 = mode3 match {
      case 0 => mInput.getOrElse(ptr + 3, 0L)
      case 1 => ptr + 3
      case 2 => mInput.getOrElse(ptr + 3, 0L) + relativeBase
    }
    (value1, value2, address3)
  }

  private def getAddress(mInput: mutable.Map[Long, Long], ptr: Long, mode1: Int, relativeBase: Long): Long = {
    mode1 match {
      case 0 => mInput(ptr + 1)
      case 1 => ptr + 1
      case 2 => mInput(ptr + 1) + relativeBase
    }
  }
}
