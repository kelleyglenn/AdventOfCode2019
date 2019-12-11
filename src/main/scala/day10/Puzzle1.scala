package day10

object Puzzle1 {
  val asteroid: Char = '#'
  val space: Char = '.'

  case class Direction(id: String, order: Short)

  val dirIds = Seq("U", "UR", "R", "DR", "D", "DL", "L", "UL")
  val dirMap: Map[String, Direction] = dirIds.zipWithIndex.map(d => (d._1, Direction(d._1, d._2.toShort))).toMap

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

    def directionTo(o: Pos): Direction = {
      val diffX = o.x - x
      val diffY = o.y - y
      val xDir: String = if (diffX > 0) "R" else if (diffX < 0) "L" else ""
      val yDir: String = if (diffY > 0) "D" else if (diffY < 0) "U" else ""
      dirMap(yDir + xDir)
    }
  }

  def canPosASeePosB(map: Seq[Seq[Char]], posA: Pos, posB: Pos): Boolean = {
    val allOtherAsteroids = allAsteroids(map).filter(p => p != posA && p != posB)
    val onSameSlope = allOtherAsteroids.filter(posA.slope(_) == posA.slope(posB))
    val sameDirection = onSameSlope.filter(posA.directionTo(_) == posA.directionTo(posB))
    val closer = sameDirection.filter(posA.distance(_) < posA.distance(posB))
    closer.isEmpty
  }

  def allVisibleAsteroidsFrom(map: Seq[Seq[Char]], pos: Pos): Seq[Pos] = {
    if (map(pos.y)(pos.x) != asteroid) throw new IllegalArgumentException("" + pos + " is not " + asteroid)
    val allOtherAsteroids = allAsteroids(map).filter(_ != pos)
    allOtherAsteroids.filter(canPosASeePosB(map, pos, _))
  }

  def numberVisible(map: Seq[Seq[Char]], pos: Pos): Int = {
    allVisibleAsteroidsFrom(map, pos).size
  }

  private def allAsteroids(map: Seq[Seq[Char]]): Seq[Pos] = {
    (for (x <- map.head.indices; y <- map.indices) yield {
      Pos(x, y)
    }).filter(p => map(p.y)(p.x) == asteroid)
  }

  def mostVisible(map: Seq[Seq[Char]]): (Int, Pos) = {
    allAsteroids(map).map(p=>(numberVisible(map, p), p)).maxBy(_._1)
  }

  def visibleSortedClockwise(map: Seq[Seq[Char]], pos: Pos): Seq[Pos] = {
    def posALessThanPosB(posA: Pos, posB: Pos): Boolean = {
      val dirToA = pos.directionTo(posA)
      val dirToB = pos.directionTo(posB)
      if (dirToA.order == dirToB.order) pos.slope(posA) < pos.slope(posB)
      else dirToA.order < dirToB.order
    }

    val visibleAsteroids = allVisibleAsteroidsFrom(map, pos)
    visibleAsteroids.sortWith(posALessThanPosB)
  }

  def explode(map: Seq[Seq[Char]], targets: Seq[Pos]): Seq[Seq[Char]] = {
    val mapArray = map.map(_.toArray).toArray
    targets.foreach(p => mapArray(p.y)(p.x) = space)
    mapArray.map(_.toSeq).toSeq
  }

  def explosionOrderFrom(map: Seq[Seq[Char]], pos: Pos): Seq[Pos] = {
    if (numberVisible(map, pos) == 0) Seq.empty else {
      val targets = visibleSortedClockwise(map, pos)
      targets ++ explosionOrderFrom(explode(map, targets), pos)
    }
  }
}
