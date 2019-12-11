package day10

import day10.Puzzle1._
import org.scalatest.flatspec.AnyFlatSpec

import scala.io.{BufferedSource, Source}

class Puzzle1Test extends AnyFlatSpec {

  class SetupTestData {
    var row0: Seq[Char] = "#..#..#".toSeq
    var row1: Seq[Char] = "...#...".toSeq
    var row2: Seq[Char] = "...#...".toSeq
    var row3: Seq[Char] = "#######".toSeq
    var row4: Seq[Char] = "...#...".toSeq
    var row5: Seq[Char] = "...#...".toSeq
    var row6: Seq[Char] = "#..#..#".toSeq
    var m = Seq(row0, row1, row2, row3, row4, row5, row6)
  }

  class SetupSmallExampleData {
    var row0: Seq[Char] = ".#..#".toSeq
    var row1: Seq[Char] = ".....".toSeq
    var row2: Seq[Char] = "#####".toSeq
    var row3: Seq[Char] = "....#".toSeq
    var row4: Seq[Char] = "...##".toSeq
    var m = Seq(row0, row1, row2, row3, row4)
  }

  class SetupPuzzleData(name: String) {
    val bufferedSource: BufferedSource = Source.fromURL(getClass.getResource("/day10/" + name + ".txt"))
    val map: Seq[Seq[Char]] = bufferedSource.getLines.toSeq.map(_.toSeq)
    bufferedSource.close()
  }

  behavior of "canPosASeePosB"
  it should "handle corner to corner" in new SetupTestData {
    assert(canPosASeePosB(m, Pos(0, 0), Pos(3, 0)))
    assert(!canPosASeePosB(m, Pos(0, 0), Pos(6, 0)))
    assert(canPosASeePosB(m, Pos(0, 0), Pos(0, 3)))
    assert(!canPosASeePosB(m, Pos(0, 0), Pos(0, 6)))
    assert(canPosASeePosB(m, Pos(0, 0), Pos(3, 3)))
    assert(!canPosASeePosB(m, Pos(0, 0), Pos(6, 6)))

    assert(canPosASeePosB(m, Pos(6, 0), Pos(3, 0)))
    assert(!canPosASeePosB(m, Pos(6, 0), Pos(0, 0)))
    assert(canPosASeePosB(m, Pos(6, 0), Pos(6, 3)))
    assert(!canPosASeePosB(m, Pos(6, 0), Pos(6, 6)))
    assert(canPosASeePosB(m, Pos(6, 0), Pos(3, 3)))
    assert(!canPosASeePosB(m, Pos(6, 0), Pos(0, 6)))

    assert(canPosASeePosB(m, Pos(0, 6), Pos(0, 3)))
    assert(!canPosASeePosB(m, Pos(0, 6), Pos(0, 0)))
    assert(canPosASeePosB(m, Pos(0, 6), Pos(3, 6)))
    assert(!canPosASeePosB(m, Pos(0, 6), Pos(6, 6)))
    assert(canPosASeePosB(m, Pos(0, 6), Pos(3, 3)))
    assert(!canPosASeePosB(m, Pos(0, 6), Pos(6, 0)))

    assert(canPosASeePosB(m, Pos(6, 6), Pos(6, 3)))
    assert(!canPosASeePosB(m, Pos(6, 6), Pos(6, 0)))
    assert(canPosASeePosB(m, Pos(6, 6), Pos(3, 6)))
    assert(!canPosASeePosB(m, Pos(6, 6), Pos(0, 6)))
    assert(canPosASeePosB(m, Pos(6, 6), Pos(3, 3)))
    assert(!canPosASeePosB(m, Pos(6, 6), Pos(0, 0)))
  }

  behavior of "numberVisible and mostVisible"
  it should "handle small example" in new SetupSmallExampleData {
    assertThrows[IllegalArgumentException] {
      numberVisible(m, Pos(0, 0))
    }
    assert(numberVisible(m, Pos(4, 2)) == 5)
    assert(numberVisible(m, Pos(0, 2)) == 6)
    assert(numberVisible(m, Pos(1, 0)) == 7)
    assert(numberVisible(m, Pos(4, 0)) == 7)
    assert(numberVisible(m, Pos(1, 2)) == 7)
    assert(numberVisible(m, Pos(2, 2)) == 7)
    assert(numberVisible(m, Pos(3, 2)) == 7)
    assert(numberVisible(m, Pos(4, 3)) == 7)
    assert(numberVisible(m, Pos(4, 4)) == 7)
    assert(numberVisible(m, Pos(3, 4)) == 8)
    assert(mostVisible(m) == 8)
  }

    it should "handle puzzle example 1" in new SetupPuzzleData("example1") {
      assert(numberVisible(map, Pos(5, 8)) == 33)
      assert(mostVisible(map) == 33)
    }

    it should "handle puzzle example 2" in new SetupPuzzleData("example2") {
      assert(numberVisible(map, Pos(1, 2)) == 35)
      assert(mostVisible(map) == 35)
    }

    it should "handle puzzle example 3" in new SetupPuzzleData("example3") {
      assert(numberVisible(map, Pos(6, 3)) == 41)
      assert(mostVisible(map) == 41)
    }

    it should "handle puzzle example 4" in new SetupPuzzleData("example4") {
      assert(numberVisible(map, Pos(11, 13)) == 210)
      assert(mostVisible(map) == 210)
    }

    it should "solve the puzzle" in new SetupPuzzleData("input") {
      assert(mostVisible(map) == 278)
    }
}
