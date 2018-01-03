package Utils

case class Direction(x: Int, y: Int) {

  def turnRight(): Direction = {
    Direction(y, -x)
  }

  def turnLeft(): Direction = {
    Direction(-y, x)
  }

  def reverse(): Direction = {
    Direction(-x, -y)
  }
}
