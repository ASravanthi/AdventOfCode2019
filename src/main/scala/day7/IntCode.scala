package day7

import scala.collection.mutable.ListBuffer
import scala.io.Source

object IntCode {

  def main(args: Array[String]): Unit = {
    val pInput = Source.fromResource("day7/input.txt").getLines().toList(0).split(",").map(_.toInt)

    println(getHighestOutputSignalSingleLoop(pInput, List(0, 1, 2, 3, 4))) //298586

    println(getHighestOutputSignalWithFeedback(pInput, List(5, 6, 7, 8, 9))) //9246095
  }

  def getHighestOutputSignalSingleLoop(pInput: Array[Int], phaseSettings: List[Int]): Int = {
    phaseSettings.permutations.map(phaseSetting => {
      val ampAOutput = new Amplifier(pInput.to[ListBuffer], phaseSetting(0), 0).getOutputSignal(0).get
      val ampBOutput = new Amplifier(pInput.to[ListBuffer], phaseSetting(1), 0).getOutputSignal(ampAOutput).get
      val ampCOutput = new Amplifier(pInput.to[ListBuffer], phaseSetting(2), 0).getOutputSignal(ampBOutput).get
      val ampDOutput = new Amplifier(pInput.to[ListBuffer], phaseSetting(3), 0).getOutputSignal(ampCOutput).get
      val ampEOutput = new Amplifier(pInput.to[ListBuffer], phaseSetting(4), 0).getOutputSignal(ampDOutput).get
      ampEOutput
    }).max
  }

  def getHighestOutputSignalWithFeedback(programSequence: Array[Int], phaseSettings: List[Int]): Int = {
    phaseSettings.permutations.map(phaseSettings => {
      val amplifiers: List[Amplifier] = phaseSettings.map(phase => new Amplifier(programSequence.to[ListBuffer], phase, 0))

      var input: Int = 0
      var output: Option[Int] = Some(input)
      while (output.isDefined) {
        amplifiers.foreach(amplifier => {
          output = amplifier.getOutputSignal(input)
          if (output.isDefined) input = output.get
        })
      }
      input
    }).max
  }

  class Amplifier(programSequence: ListBuffer[Int], phaseSetting: Int, var position: Int) {

    def getOutputSignal(input: Int): Option[Int] = {
      while (position < programSequence.size) {
        val instruction = "%05d".format(programSequence(position))

        val opcode = instruction.substring(3, 5).toInt
        val mode1 = instruction.substring(2, 3).toInt
        val mode2 = instruction.substring(1, 2).toInt

        opcode match {
          case 1 => {
            val (value1, value2) = getValues(programSequence.toList, position, mode1, mode2)
            programSequence(programSequence(position + 3)) = value1 + value2
            position += 4
          }
          case 2 => {
            val (value1, value2) = getValues(programSequence.toList, position, mode1, mode2)
            programSequence(programSequence(position + 3)) = value1 * value2
            position += 4
          }
          case 99 => {
            return None
          }
          case 3 => {
            if (position == 0) {
              programSequence(programSequence(position + 1)) = phaseSetting
            } else {
              programSequence(programSequence(position + 1)) = input
            }
            position += 2
          }
          case 4 => {
            val output = Some(programSequence(programSequence(position + 1)))
            position += 2
            return output
          }
          case 5 =>
            val (value1, value2) = getValues(programSequence.toList, position, mode1, mode2)
            if (value1 != 0) {
              position = value2
            } else {
              position += 3
            }
          case 6 =>
            val (value1, value2) = getValues(programSequence.toList, position, mode1, mode2)
            if (value1 == 0) {
              position = value2
            } else {
              position += 3
            }
          case 7 =>
            val (value1, value2) = getValues(programSequence.toList, position, mode1, mode2)
            if (value1 < value2) {
              programSequence(programSequence(position + 3)) = 1
            } else {
              programSequence(programSequence(position + 3)) = 0
            }
            position += 4
          case 8 =>
            val (value1, value2) = getValues(programSequence.toList, position, mode1, mode2)
            if (value1 == value2) {
              programSequence(programSequence(position + 3)) = 1
            } else {
              programSequence(programSequence(position + 3)) = 0
            }
            position += 4
        }
      }
      return None
    }
  }

  private def getValues(mInput: List[Int], ptr: Int, mode1: Int, mode2: Int): (Int, Int) = {
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
