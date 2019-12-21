package day6

import scala.io.Source

object OrbitMap {

  def main(args: Array[String]): Unit = {
    val inputData: Map[String, String] = Source.fromResource("day6/input.txt").getLines().toList
      .map(line => {
        val splitLine = line.split("\\)")
        (splitLine(1), splitLine(0))
      }).toMap

    println(getTotalOrbits(inputData)) //117672
  }

  private def getTotalOrbits(inputData: Map[String, String]): Int = {
    inputData.map { case (orbiter, orbital) => {
      var orbitCount = 1 //direct orbit

      if (orbital != "COM") {
        var vOrbital = orbital
        while (inputData.contains(vOrbital)) {
          orbitCount += 1
          vOrbital = inputData(vOrbital)
        }
      }
      orbitCount //indirect orbit
    }
    }.sum
  }
}
