package day7

import org.scalatest.flatspec.AnyFlatSpec

import scala.io.{BufferedSource, Source}

class Puzzle1Test extends AnyFlatSpec {

  class SetupData {
    val program1: Seq[Int] = Seq(3, 15, 3, 16, 1002, 16, 10, 16, 1, 16, 15, 15, 4, 15, 99, 0, 0)
    val program2: Seq[Int] = Seq(3, 23, 3, 24, 1002, 24, 10, 24, 1002, 23, -1, 23,
      101, 5, 23, 23, 1, 24, 23, 23, 4, 23, 99, 0, 0)
    val program3: Seq[Int] = Seq(3, 31, 3, 32, 1002, 32, 10, 32, 1001, 31, -2, 31, 1007, 31, 0, 33,
      1002, 33, 7, 33, 1, 33, 31, 31, 1, 32, 31, 31, 4, 31, 99, 0, 0, 0)
  }

  class SetupPuzzleData {
    val bufferedSource: BufferedSource = Source.fromURL(getClass.getResource("/day7/input.txt"))
    val puzzleString: String = bufferedSource.getLines.toSeq.head
    val puzzleProgram: Seq[Int] = puzzleString.split(',').map(_.toInt).toSeq
    bufferedSource.close()
  }

  behavior of "processWithSettings"
  it should "handle puzzle examples" in new SetupData {
    assert(Puzzle1.processWithSettings(program1, Seq(4, 3, 2, 1, 0)) == 43210)
    assert(Puzzle1.processWithSettings(program1, Seq(4, 4, 4, 4, 4)) == 44444)
    assert(Puzzle1.processWithSettings(program2, Seq(0, 1, 2, 3, 4)) == 54321)
    assert(Puzzle1.processWithSettings(program2, Seq(0, 0, 0, 0, 0)) == 55555)
    assert(Puzzle1.processWithSettings(program3, Seq(1, 0, 4, 3, 2)) == 65210)
    assert(Puzzle1.processWithSettings(program3, Seq(1, 1, 1, 1, 1)) == 66666)
  }

  behavior of "findSettingsWithHighestOutput"
  it should "handle puzzle examples" in new SetupData {
    assert(Puzzle1.findSettingsWithHighestOutput(program1) == 43210)
    assert(Puzzle1.findSettingsWithHighestOutput(program2) == 54321)
    assert(Puzzle1.findSettingsWithHighestOutput(program3) == 65210)
  }

  it should "solve puzzle" in new SetupPuzzleData {
    assert(Puzzle1.findSettingsWithHighestOutput(puzzleProgram) == 437860)
  }
}
