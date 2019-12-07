package day6

object Puzzle1 {

  def countOrbitsForChild(m: Map[String, String], c: String): Int = {
    if (m.get(c).isEmpty) 0 else
      1 + countOrbitsForChild(m, m(c))
  }

  def countAllOrbits(m: Map[String, String]): Int = {
    m.keySet.toSeq.map(countOrbitsForChild(m, _)).sum
  }

  def setOfAncestors(m: Map[String, String], c: String): Set[String] = {
    if (m.get(c).isEmpty) Set.empty else
      setOfAncestors(m, m(c)) + m(c)
  }

  def setOfCommonAncestors(m: Map[String, String], a: String, b: String): Set[String] = {
    setOfAncestors(m, a).intersect(setOfAncestors(m, b))
  }

  def distanceToAncestor(m: Map[String, String], c: String, a: String): Int = {
    if (c == a) 0 else 1 + distanceToAncestor(m, m(c), a)
  }

  def setOfCommonAncestorsWithDistances(m: Map[String, String], a: String, b: String): Set[(String, Int, Int)] = {
    setOfCommonAncestors(m, a, b).map(x => (x, distanceToAncestor(m, a, x), distanceToAncestor(m, b, x)))
  }

  def distanceBetweenNodes(m: Map[String, String], a: String, b: String): Int = {
    // Make sure to handle the case where one is the ancestor of the other
    if (setOfAncestors(m, a).contains(b)) distanceToAncestor(m, a, b)
    else if (setOfAncestors(m, b).contains(a)) distanceToAncestor(m, b, a)
    else setOfCommonAncestorsWithDistances(m, a, b).map(x => x._2 + x._3).min
  }
}
