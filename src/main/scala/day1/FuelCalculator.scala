package day1

import scala.io.Source

object FuelCalculator {

  def main(args: Array[String]): Unit = {
    val mass = Source.fromResource("day1/input.txt").getLines.toList.map(_.toInt)

    //part1
    println(mass.map(calculateFuel(_)).reduce(_ + _)) // 3265923

    //part2
    println(mass.map(m => calculationAdditionalFuel(m, 0)).reduce(_ + _)) //4896020
  }

  private def calculateFuel(mass: Double) = Math.floor(mass / 3) - 2

  private def calculationAdditionalFuel(mass: Double, sum: Double): Double = {
    val fuel = calculateFuel(mass)
    if (fuel <= 0) sum else calculationAdditionalFuel(fuel, fuel + sum)
  }

}
