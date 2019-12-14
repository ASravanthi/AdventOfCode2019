package day3

case class Coordinate(x: Int, y: Int, stepsTakenFromCentralPort: Int) {

  def moveRight(steps: Int): List[Coordinate] = {
    List.range(1, steps + 1).map(i => Coordinate(x + i, y, this.stepsTakenFromCentralPort + i))
  }

  def moveLeft(steps: Int): List[Coordinate] = {
    List.range(1, steps + 1).map(i => Coordinate(x - i, y, this.stepsTakenFromCentralPort + i))
  }

  def moveUp(steps: Int): List[Coordinate] = {
    List.range(1, steps + 1).map(i => Coordinate(x, y + i, this.stepsTakenFromCentralPort + i))
  }

  def moveDown(steps: Int): List[Coordinate] = {
    List.range(1, steps + 1).map(i => Coordinate(x, y - i, this.stepsTakenFromCentralPort + i))
  }

  override def toString(): String = "(" + x + ", " + y + ")"

  override def equals(that: Any): Boolean = {
    that match {
      case that: Coordinate => this.x == that.x && this.y == that.y
      case _ => false
    }
  }

  override def hashCode: Int = 41 * (41 + x) + y
}
