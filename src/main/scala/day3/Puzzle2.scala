package day3

object Puzzle2 {
  def lineStringToPath(line: String): Seq[(Direction.Value, Int)] = {
    line.split(',').map { s: String =>
      (Direction.withName(s.head.toString), s.tail.toInt)
    }.toSeq
  }

  def pointsWithDistanceAlongPath(path: Seq[(Direction.Value, Int)], curPointDist: ((Int, Int), Int)): Map[(Int, Int), Int] = {
    if (path.size < 1) Map.empty
    else {
      val newPointsWithDist: Seq[((Int, Int), Int)] = for (dist <- 1 to path.head._2) yield ( {
        path.head._1 match {
          case Direction.U => (curPointDist._1._1, curPointDist._1._2 + dist)
          case Direction.D => (curPointDist._1._1, curPointDist._1._2 - dist)
          case Direction.L => (curPointDist._1._1 - dist, curPointDist._1._2)
          case Direction.R => (curPointDist._1._1 + dist, curPointDist._1._2)
        }
      }, curPointDist._2 + dist)
      pointsWithDistanceAlongPath(path.tail, newPointsWithDist.last) ++ newPointsWithDist.toMap
    }
  }

  def sharedPointWithFewestSteps(a: Map[(Int, Int), Int], b: Map[(Int, Int), Int]): Int = {
    val sharedKeys = a.keySet.intersect(b.keySet)
    sharedKeys.map(k => {
      a(k) + b(k)
    }).min
  }

  def distanceClosestSharedPointFromStrings(line1: String, line2: String): Int = {
    val points1 = pointsWithDistanceAlongPath(lineStringToPath(line1), ((0,0),0))
    val points2 = pointsWithDistanceAlongPath(lineStringToPath(line2), ((0,0),0))
    sharedPointWithFewestSteps(points1, points2)
  }
}
