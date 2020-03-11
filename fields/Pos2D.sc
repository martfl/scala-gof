import scala.util.Random

class Dir2D(val x: Double, val y: Double) {
  def turnRight: Dir2D = new Dir2D(-y, x)
  def turnLeft: Dir2D = new Dir2D(y, -x)
  def turnAround: Dir2D = new Dir2D(-x, -y)

  def crossZ(other: Dir2D): Double = x * (other.y - y) - y * (other.x - x)

  def rightOf(other: Dir2D): Boolean = crossZ(other) < 0
  def leftOf(other: Dir2D): Boolean = crossZ(other) > 0

  def approx4: Dir2D = Dir2D.approx4(x, y)
  def approx8: Dir2D = Dir2D.approx8(x, y)

  override def equals(obj: scala.Any): Boolean = obj match {
    case dir: Dir2D => dir.x == x && dir.y == y
    case _ => false
  }

  override def hashCode: Int = x.hashCode + y.hashCode
  override def toString: String = s"Dir2D(${x.round(3)}, ${y.round(3)})"
}

object Dir2D {
  val dirs4 = 4
}

case class Pos2D(x: Int, y: Int) {
  def move(dir: Dir2D): Pos2D = copy(x + dir.x.toInt, y + dir.y.toInt)

  def dir(other: Pos2D) = Dir2D(other.x - x, other.y - y)
  def -(other: Pos2D) = other.dir(this)
}

object Pos2D {
  def apply(dim: Int): List[Pos2D] = 
    (0 until dim).flatMap(x => (0 until dim).map(Pos2D(x, _))).toList
  def range(p1: Pos2D, p2: Pos2D): List[Pos2D] = {
    val xfrom = math.min(p1.x, p2.x)
    val yfrom = math.min(p1.y, p2.y)
    val xto = math.max(p1.x, p2.x)
    val yto = math.max(p1.y, p2.y)

    (xfrom to xto).flatMap(x => (yfrom to yto).map(Pos2D(x, _))).toList
  }
  def random(dim: Int): Pos2D = Pos2D(Random.nextInt(dim), Random.nextInt(dim))
}
