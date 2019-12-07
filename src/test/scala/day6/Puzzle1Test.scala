package day6

import org.scalatest.flatspec.AnyFlatSpec

import scala.io.{BufferedSource, Source}

class Puzzle1Test extends AnyFlatSpec {
  def stringsToParentChildPairs(pairs: Seq[String]): Seq[(String, String)] = {
    pairs.map(pair => {
      val p = pair.split(')')
      (p(0), p(1))
    })
  }

  behavior of "stringsToParentChildPairs"
  it should "handle empty" in {
    assert(stringsToParentChildPairs(Seq.empty) == Seq.empty)
  }

  it should "handle one" in {
    assert(stringsToParentChildPairs(Seq("A)B")) == Seq(("A", "B")))
  }

  it should "handle two" in {
    assert(stringsToParentChildPairs(Seq("COM)B", "B)C")) == Seq(("COM", "B"), ("B", "C")))
  }

  class SetupData {
    val exampleSeq: Seq[String] = Seq("COM)B", "B)C", "C)D", "D)E", "E)F", "B)G", "G)H", "D)I", "E)J", "J)K", "K)L")
    val pcs: Seq[(String, String)] = stringsToParentChildPairs(exampleSeq)
    val m: Map[String, String] = pcs.map(_.swap).toMap
  }

  class SetupPuzzleData {
    val bufferedSource: BufferedSource = Source.fromURL(getClass.getResource("/day6/input.txt"))
    val puzzleSeq: Seq[String] = bufferedSource.getLines.toSeq
    val puzzlePCs: Seq[(String, String)] = stringsToParentChildPairs(puzzleSeq)
    val puzzleMap: Map[String, String] = puzzlePCs.map(_.swap).toMap
    bufferedSource.close()
  }

  behavior of "countOrbitsForChild"
  it should "handle one" in new SetupData {
    assert(Puzzle1.countOrbitsForChild(m, "B") == 1)
  }

  it should "handle two" in new SetupData {
    assert(Puzzle1.countOrbitsForChild(m, "C") == 2)
  }

  it should "handle puzzle examples" in new SetupData {
    assert(Puzzle1.countOrbitsForChild(m, "D") == 3)
    assert(Puzzle1.countOrbitsForChild(m, "L") == 7)
    assert(Puzzle1.countOrbitsForChild(m, "COM") == 0)
  }

  behavior of "countAllOrbits"
  it should "handle puzzle examples" in new SetupData {
    assert(Puzzle1.countAllOrbits(m) == 42)
  }

  it should "solve the puzzle" in new SetupPuzzleData {
    assert(Puzzle1.countAllOrbits(puzzleMap) == 402879)
  }

  behavior of "setOfAncestors"
  it should "handle empty" in {
    assert(Puzzle1.setOfAncestors(Map.empty, "Any") == Set.empty)
  }

  it should "handle root" in new SetupData {
    assert(Puzzle1.setOfAncestors(m, "COM") == Set.empty)
  }

  it should "handle one" in new SetupData {
    assert(Puzzle1.setOfAncestors(m, "B") == Set("COM"))
  }

  it should "handle many" in new SetupData {
    assert(Puzzle1.setOfAncestors(m, "F") == Set("COM", "B", "C", "D", "E"))
    assert(Puzzle1.setOfAncestors(m, "H") == Set("COM", "B", "G"))
    assert(Puzzle1.setOfAncestors(m, "I") == Set("COM", "B", "C", "D"))
    assert(Puzzle1.setOfAncestors(m, "L") == Set("COM", "B", "C", "D", "E", "J", "K"))
  }

  behavior of "setOfCommonAncestors"
  it should "be reflexive" in new SetupData {
    assert(Puzzle1.setOfCommonAncestors(m, "L", "L") == Puzzle1.setOfAncestors(m, "L"))
  }

  it should "handle puzzle example" in new SetupData {
    assert(Puzzle1.setOfCommonAncestors(m, "F", "H") == Set("COM", "B"))
    assert(Puzzle1.setOfCommonAncestors(m, "F", "L") == Set("COM", "B", "C", "D", "E"))
  }

  behavior of "distanceToAncestor"
  it should "handle self" in new SetupData {
    assert(Puzzle1.distanceToAncestor(m, "B", "B") == 0)
  }

  it should "handle parent" in new SetupData {
    assert(Puzzle1.distanceToAncestor(m, "B", "COM") == 1)
  }

  it should "handle distant" in new SetupData {
    assert(Puzzle1.distanceToAncestor(m, "L", "COM") == 7)
    assert(Puzzle1.distanceToAncestor(m, "F", "COM") == 5)
  }

  behavior of "setOfCommonAncestorsWithDistances"
  it should "handle puzzle examples" in new SetupData {
    assert(Puzzle1.setOfCommonAncestorsWithDistances(m, "F", "H") == Set(("COM", 5, 3), ("B", 4, 2)))
  }

  behavior of "distanceBetweenNodes"
  it should "handle ancestors" in new SetupData {
    assert(Puzzle1.distanceBetweenNodes(m, "COM", "B") == 1)
    assert(Puzzle1.distanceBetweenNodes(m, "B", "C") == 1)
    assert(Puzzle1.distanceBetweenNodes(m, "C", "L") == 5)
  }

  it should "handle puzzle example" in new SetupData {
    assert(Puzzle1.distanceBetweenNodes(m, "K", "I") == 4)
  }

  it should "solve the puzzle" in new SetupPuzzleData {
    assert(Puzzle1.distanceBetweenNodes(puzzleMap, puzzleMap("YOU"), puzzleMap("SAN")) == 484)
  }
}
