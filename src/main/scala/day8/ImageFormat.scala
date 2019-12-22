package day8

import scala.io.Source

object ImageFormat {

  def main(args: Array[String]): Unit = {
    val pixels = Source.fromResource("day8/input.txt").getLines().toList(0).toCharArray.map(_.asDigit)
    val image = Image(pixels, 25, 6)

    val layerWithFewest0 = image.findLayerWithFewestDigitsOf(0)
    println(Image.findDiagCode(layerWithFewest0)) //2048

    Image.print(image.buildImageWithColors)
  }

}
