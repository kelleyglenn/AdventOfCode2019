package day7

import java.util.concurrent.{BlockingQueue, LinkedBlockingQueue}

import org.scalatest.flatspec.AnyFlatSpec

import scala.io.{BufferedSource, Source}
import scala.jdk.CollectionConverters._

class Puzzle2Test extends AnyFlatSpec {
  behavior of "Computer"

  def testMemoryWrites(source: Seq[Int], expectedProgram: Seq[Int]): Unit = {
    val c: Puzzle2.Computer = new Puzzle2.Computer(source.toArray)
    c.run()
    c.join()
    assert(c.memory.toIndexedSeq == expectedProgram)
  }

  def testIO(source: Seq[Int], input: Seq[Int], expectedOutput: Seq[Int]): Unit = {
    val inQ: BlockingQueue[Int] = if (input.nonEmpty) new LinkedBlockingQueue[Int](input.size) else null
    val outQ: BlockingQueue[Int] = new LinkedBlockingQueue[Int](expectedOutput.size)
    val c: Puzzle2.Computer = new Puzzle2.Computer(source.toArray, inQ, outQ)
    c.start()
    for (i <- input) {
      inQ.put(i)
    }
    c.join()
    assert(outQ.asScala.toSeq == expectedOutput)
  }

  it should "still work like day5" in {
    testMemoryWrites(Seq(1, 0, 0, 0, 99), Seq(2, 0, 0, 0, 99))
    testMemoryWrites(Seq(2, 3, 0, 3, 99), Seq(2, 3, 0, 6, 99))
    testIO(Seq(3, 0, 4, 0, 99), Seq(123), Seq(123))
    testMemoryWrites(Seq(1002, 4, 3, 4, 33), Seq(1002, 4, 3, 4, 99))
    testIO(Seq(104, 321, 99), Seq.empty, Seq(321))
    testMemoryWrites(Seq(1101, 100, -1, 4, 0), Seq(1101, 100, -1, 4, 99))
    val program1 = Seq(3, 9, 8, 9, 10, 9, 4, 9, 99, -1, 8)
    testIO(program1, Seq(1), Seq(0))
    testIO(program1, Seq(8), Seq(1))
    val program2 = Seq(3, 9, 7, 9, 10, 9, 4, 9, 99, -1, 8)
    testIO(program2, Seq(1), Seq(1))
    testIO(program2, Seq(8), Seq(0))
    val program3 = Seq(3, 12, 6, 12, 15, 1, 13, 14, 13, 4, 13, 99, -1, 0, 1, 9)
    testIO(program3, Seq(0), Seq(0))
    testIO(program3, Seq(100), Seq(1))
    val program4 = Seq(3, 3, 1105, -1, 9, 1101, 0, 0, 12, 4, 12, 99, 1)
    testIO(program4, Seq(0), Seq(0))
    testIO(program4, Seq(100), Seq(1))
  }

  class SetupData {
    val program1: Seq[Int] = Seq(3, 26, 1001, 26, -4, 26, 3, 27, 1002, 27, 2, 27, 1, 27, 26, 27, 4, 27,
      1001, 28, -1, 28, 1005, 28, 6, 99, 0, 0, 5)
    val program2: Seq[Int] = Seq(3, 52, 1001, 52, -5, 52, 3, 53, 1, 52, 56, 54, 1007, 54, 5, 55, 1005, 55, 26,
      1001, 54, -5, 54, 1105, 1, 12, 1, 53, 54, 53, 1008, 54, 0, 55, 1001, 55, 1, 55, 2, 53, 55, 53, 4, 53,
      1001, 56, -1, 56, 1005, 56, 6, 99, 0, 0, 0, 0, 10)
  }

  class SetupPuzzleData {
    val bufferedSource: BufferedSource = Source.fromURL(getClass.getResource("/day7/input.txt"))
    val puzzleString: String = bufferedSource.getLines.toSeq.head
    val puzzleProgram: Seq[Int] = puzzleString.split(',').map(_.toInt).toSeq
    bufferedSource.close()
  }

  behavior of "processWithFeedback"
  it should "handle examples" in new SetupData {
    assert(Puzzle2.processWithFeedback(program1, Seq(9, 8, 7, 6, 5)) == 139629729)
    assert(Puzzle2.processWithFeedback(program2, Seq(9, 7, 8, 5, 6)) == 18216)
  }

  behavior of "findSettingsWithHighestOutput"
  it should "handle examples" in new SetupData {
    assert(Puzzle2.findSettingsWithHighestOutput(program1) == 139629729)
    assert(Puzzle2.findSettingsWithHighestOutput(program2) == 18216)
  }

  it should "solve the puzzle" in new SetupPuzzleData {
    assert(Puzzle2.findSettingsWithHighestOutput(puzzleProgram) == 49810599)
  }
}
