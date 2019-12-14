package day3

import scala.io.Source
import Direction._

object CircuitBreaker {

  def main(args: Array[String]): Unit = {
    val inputData = Source.fromResource("day3/input.txt").getLines().toList
    val w1Paths: Array[String] = inputData(0).split(",")
    val w2Paths: Array[String] = inputData(1).split(",")
    val centralPort = Coordinate(0, 0, 0)

    val coordinatesCoveredW1 = getCoordinatesCovered(w1Paths, centralPort)
    val coordinatesCoveredW2 = getCoordinatesCovered(w2Paths, centralPort)
    val crossLocations = coordinatesCoveredW1.intersect(coordinatesCoveredW2)

    //part1
    println(crossLocations.map(getManhattanDistance(centralPort, _)).reduce(_ min _)) // 308

    //part2
    println(crossLocations.map(c => {
      coordinatesCoveredW1.filter(_ == c)(0).stepsTakenFromCentralPort + coordinatesCoveredW2.filter(_ == c)(0).stepsTakenFromCentralPort
    }).reduce(_ min _)) //12934
  }

  private def getManhattanDistance(from: Coordinate, to: Coordinate) = {
    Math.abs(from.x - to.x) + Math.abs(from.y - to.y)
  }

  private def getCoordinatesCovered(paths: Array[String], centralPort: Coordinate): Array[Coordinate] = {
    var latestCoordinate = centralPort
    paths.flatMap(path => {
      val direction = Direction.withName(path.substring(0, 1))
      val distance = path.substring(1).toInt

      val covered: List[Coordinate] = direction match {
        case RIGHT => latestCoordinate.moveRight(distance)
        case LEFT => latestCoordinate.moveLeft(distance)
        case UP => latestCoordinate.moveUp(distance)
        case DOWN => latestCoordinate.moveDown(distance)
      }

      latestCoordinate = covered.last
      covered
    })
  }
}