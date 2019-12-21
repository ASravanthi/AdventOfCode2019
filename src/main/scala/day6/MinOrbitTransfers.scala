package day6

import scala.io.Source

object MinOrbitTransfers {

  def main(args: Array[String]): Unit = {
    val inputData: Map[String, String] = Source.fromResource("day6/input.txt").getLines().toList
      .map(line => {
        val splitLine = line.split("\\)")
        (splitLine(1), splitLine(0))
      }).toMap

    println(getMinOrbitTransfers(inputData,  "YOU", "SAN"))
  }

  private def getMinOrbitTransfers(inputData: Map[String, String], source: String, destn: String): Int = {
    val sourceOrbiting = inputData(source)
    val destnOrbiting = inputData(destn)

    val sourcePathToCOM = findPath(inputData, sourceOrbiting)
    val destnPathToCOM = findPath(inputData, destnOrbiting)

    sourcePathToCOM.indices.foreach(s => {
      if (destnPathToCOM.contains(sourcePathToCOM(s))) {
        return s + destnPathToCOM.indexOf(sourcePathToCOM(s)) + 2
      }
    })
    0
  }

  private def findPath(inputData: Map[String, String], orbit: String): List[String] = {
    var path = List.empty[String]
    var vOrbit = orbit
    while (inputData.contains(vOrbit)) {
      path = path :+ inputData(vOrbit)
      vOrbit = inputData(vOrbit)
    }
    path
  }

}
