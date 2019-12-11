package day10

object Puzzle1 {
  val asteroid: Char = '#'

  case class Pos(x: Int, y: Int) {
    def slope(o: Pos): Double = {
      val diffX: Double = o.x - x
      val diffY: Double = o.y - y
      if (diffX == 0) Int.MaxValue else diffY / diffX
    }

    def distance(o: Pos): Double = {
      val diffX = o.x - x
      val diffY = o.y - y
      Math.sqrt(diffX * diffX + diffY * diffY)
    }

    def directionTo(o: Pos): String = {
      val diffX = o.x - x
      val diffY = o.y - y
      val xDir: String = if (diffX > 0) "R" else if (diffX < 0) "L" else ""
      val yDir: String = if (diffY > 0) "D" else if (diffY < 0) "U" else ""
      xDir + yDir
    }
  }

  def canPosASeePosB(map: Seq[Seq[Char]], posA: Pos, posB: Pos): Boolean = {
    val allOtherAsteroids = allAsteroids(map).filter(p => p != posA && p != posB)
    val onSameSlope = allOtherAsteroids.filter(posA.slope(_) == posA.slope(posB))
    val sameDirection = onSameSlope.filter(posA.directionTo(_) == posA.directionTo(posB))
    val closer = sameDirection.filter(posA.distance(_) < posA.distance(posB))
    closer.isEmpty
  }

  def numberVisible(map: Seq[Seq[Char]], pos: Pos): Int = {
    if (map(pos.y)(pos.x) != asteroid) throw new IllegalArgumentException("" + pos + " is not " + asteroid)
    val allOtherAsteroids = allAsteroids(map).filter(_ != pos)
    val allVisibleAsteroids = allOtherAsteroids.filter(canPosASeePosB(map, pos, _))
    allVisibleAsteroids.size
  }

  private def allAsteroids(map: Seq[Seq[Char]]) = {
    (for (x <- map.head.indices; y <- map.indices) yield {
      Pos(x, y)
    }).filter(p => map(p.y)(p.x) == asteroid)
  }

  def mostVisible(map: Seq[Seq[Char]]): Int = {
    allAsteroids(map).map(numberVisible(map, _)).max
  }
}
