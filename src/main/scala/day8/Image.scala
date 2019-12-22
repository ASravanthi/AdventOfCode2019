package day8

case class Image(pixels: Array[Array[Array[Int]]]) {

  def getLayerTallWideDimensions: (Int, Int, Int) = {
    (pixels.length, pixels(0).length, pixels(0)(0).length)
  }

  def buildImageWithColors(): Array[Array[Int]] = {
    val (layersCount, height, width) = getLayerTallWideDimensions

    val colorImage = Array.ofDim[Int](height, width)
    for (hi <- 0 until height; wi <- 0 until width) {
      val pixelsFromAllLayers = (0 until layersCount).map(l => {
        pixels(l)(hi)(wi)
      })

      val TRANSPARENTCOLOR = 2
      val color = pixelsFromAllLayers.filterNot(_ == TRANSPARENTCOLOR).head
      colorImage(hi)(wi) = color
    }
    colorImage
  }

  def findLayerWithFewestDigitsOf(digit: Int): Array[Array[Int]] = {
    pixels.minBy(layer => {
      layer.flatten.filter(_ == digit).length
    })
  }
}

object Image {
  def apply(pixels: Array[Int], wide: Int, tall: Int): Image = {
    new Image(buildImage(pixels, 25, 6))
  }

  private def buildImage(pixels: Array[Int], wide: Int, tall: Int): Array[Array[Array[Int]]] = {
    val layers = pixels.length / (wide * tall)
    val image = Array.ofDim[Int](layers, tall, wide)

    val pixelRows = pixels.grouped(wide)
    while (pixelRows.hasNext) {
      for {
        layer <- 0 until layers
        height <- 0 until tall
      } {
        image(layer)(height) = pixelRows.next()
      }
    }
    image
  }

  def findDiagCode(layer: Array[Array[Int]]) = {
    layer.flatten.filter(_ == 1).size * layer.flatten.filter(_ == 2).size
  }

  def print(image: Array[Array[Int]]): Unit = {
    for (hi <- 0 until image.length) {
      for (wi <- 0 until image(0).length) {
        if (image(hi)(wi) == 1) {
          scala.Predef.print("* ")
        } else {
          scala.Predef.print("  ")
        }
      }
      println()
    }
  }

}
