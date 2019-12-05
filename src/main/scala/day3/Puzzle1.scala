package day3

object Puzzle1 {
  def pointsAlongPath(path: Seq[(Direction.Value, Int)], curPoint: (Int, Int) = (0, 0)): Set[(Int, Int)] = {
    if (path.size < 1) Set.empty
    else {
      val newPoints: Seq[(Int, Int)] = for (dist <- 1 to path.head._2) yield {
        path.head._1 match {
          case Direction.U => (curPoint._1, curPoint._2 + dist)
          case Direction.D => (curPoint._1, curPoint._2 - dist)
          case Direction.L => (curPoint._1 - dist, curPoint._2)
          case Direction.R => (curPoint._1 + dist, curPoint._2)
        }
      }
      newPoints.toSet ++ pointsAlongPath(path.tail, newPoints.last)
    }
  }

  def sharedPointClosestToOrigin(a: Set[(Int, Int)], b: Set[(Int, Int)]): (Int, Int) = {
    val shared = a.intersect(b).toSeq
    shared.zip(shared.map(manhattanDistance(_))).sortBy(_._2).head._1
  }

  def manhattanDistance(a: (Int, Int)): Int = {
    math.abs(a._1) + math.abs(a._2)
  }

  def lineStringToPath(line: String): Seq[(Direction.Value, Int)] = {
    line.split(',').map { (s: String) =>
      (Direction.withName(s.head.toString), s.tail.toInt)
    }.toSeq
  }

  def distanceClosestSharedPointFromStrings(line1: String, line2: String): Int = {
    val points1 = pointsAlongPath(lineStringToPath(line1), (0,0))
    val points2 = pointsAlongPath(lineStringToPath(line2), (0,0))
    manhattanDistance(sharedPointClosestToOrigin(points1, points2))
  }
}

object Direction extends Enumeration {
  val U: Direction.Value = Value("U")
  val D: Direction.Value = Value("D")
  val L: Direction.Value = Value("L")
  val R: Direction.Value = Value("R")
}
