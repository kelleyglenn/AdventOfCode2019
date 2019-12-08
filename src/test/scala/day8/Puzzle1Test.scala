package day8

import day8.Puzzle1._
import org.scalatest.flatspec.AnyFlatSpec

import scala.io.{BufferedSource, Source}

class Puzzle1Test extends AnyFlatSpec {
  behavior of "seqToLayers"
  it should "handle different sizes and shapes" in {
    assert(Puzzle1.seqToLayers("A".toSeq, (1, 1)) == Seq("A".toSeq))
    assert(Puzzle1.seqToLayers("ABCD".toSeq, (1, 1)) == Seq("A".toSeq, "B".toSeq, "C".toSeq, "D".toSeq))
    assert(Puzzle1.seqToLayers("ABCD".toSeq, (2, 1)) == Seq("AB".toSeq, "CD".toSeq))
  }

  class SetupPuzzleData {
    val bufferedSource: BufferedSource = Source.fromURL(getClass.getResource("/day8/input.txt"))
    val puzzleString: String = bufferedSource.getLines.toSeq.head
    bufferedSource.close()
  }

  behavior of "findLayerWithLeast"
  it should "handle alpha and numeric" in {
    assert(Puzzle1.findLayerWithLeast(Seq("AB".toSeq, "CD".toSeq), 'A') == "CD".toSeq)
    assert(Puzzle1.findLayerWithLeast(Seq("104065".toSeq, "010406".toSeq, "587210".toSeq), '0') == "587210".toSeq)
  }

  it should "solve the puzzle" in new SetupPuzzleData {
    val layers: Seq[Seq[Char]] = Puzzle1.seqToLayers(puzzleString.toSeq, (25, 6))
    val layer: Seq[Char] = Puzzle1.findLayerWithLeast(layers, '0')
    val answer: Int = layer.count(_ == '1') * layer.count(_ == '2')
    assert(answer == 1340)
  }

  behavior of "combineLayers"
  it should "handle puzzle example" in {
    assert(combineLayers(Puzzle1.seqToLayers("0222112222120000".toSeq, (2,2))) == "0110".toSeq)
  }

  it should "solve the puzzle" in new SetupPuzzleData {
    layerToLines(combineLayers(seqToLayers(puzzleString.toSeq, (25, 6))), lineWidth = 25).foreach(println(_))
  }
}
