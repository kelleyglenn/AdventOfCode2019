package day9

import java.util.concurrent.{BlockingQueue, LinkedBlockingQueue}

import day9.Puzzle1._
import org.scalatest.flatspec.AnyFlatSpec

import scala.collection.mutable.ArrayBuffer
import scala.io.{BufferedSource, Source}
import scala.jdk.CollectionConverters._

class Puzzle1Test extends AnyFlatSpec {
  def testOutput(source: Seq[Long], expectedOutput: Seq[Long], memSize: Int = 100): Unit = {
    testIO(source, memSize, expectedOutput = expectedOutput)
  }

  // This is here so I can see the computer's output while it's still running
  class IOTeeToPrintln(val inQ: BlockingQueue[Long], val outQ: BlockingQueue[Long]) extends Thread {
    var shouldEnd: Boolean = false

    def handleOne(): Unit = {
      val l: Long = inQ.take()
      //println("Out=" + l)
      outQ.put(l)
    }

    override def run(): Unit = {
      while (!shouldEnd) {
        if (!inQ.isEmpty)
          handleOne()
        else Thread.sleep(10)
      }
      // flush
      while (!inQ.isEmpty)
        handleOne()
    }
  }

  def testIO(source: Seq[Long], memSize: Int = 100, input: Seq[Long] = Seq.empty, expectedOutput: Seq[Long]): Unit = {
    val inQ: BlockingQueue[Long] = if (input.nonEmpty) new LinkedBlockingQueue[Long](input.size) else null
    val memory: ArrayBuffer[Long] = {
      val m: ArrayBuffer[Long] = ArrayBuffer.from(source.toArray)
      m.addAll(new Array[Long](memSize))
    }
    val c: Computer = new Computer(memory.toArray, inQ, new LinkedBlockingQueue[Long](expectedOutput.size))
    val t: IOTeeToPrintln = new IOTeeToPrintln(c.output, new LinkedBlockingQueue[Long](expectedOutput.size))
    c.start()
    t.start()
    for (i <- input) {
      inQ.put(i)
    }
    c.join()
    t.shouldEnd = true
    t.join()
    assert(t.outQ.asScala.toSeq == expectedOutput)
  }

  class SetupPuzzleData {
    val bufferedSource: BufferedSource = Source.fromURL(getClass.getResource("/day9/input.txt"))
    val puzzleString: String = bufferedSource.getLines.toSeq.head
    val puzzleProgram: Seq[Long] = puzzleString.split(',').map(_.toLong).toSeq
    bufferedSource.close()
  }

  behavior of "Computer"
  it should "handle new mode, and extended memory" in {
    testOutput(Seq(204, 1, 99), Seq(1))
    testOutput(Seq(204, 2, 99), Seq(99))
    testOutput(Seq(204, 3, 99), Seq(0))
    testOutput(Seq(204, 1000, 99), Seq(0), 1000)
    testIO(Seq(203, 1, 204, 1, 99), 0, Seq(123), Seq(123))
  }
  it should "handle new op" in {
    testOutput(Seq(9, 1, 204, 1, 99), Seq(204))
    testOutput(Seq(109, 1, 204, 1, 99), Seq(204))
    testOutput(Seq(209, 1, 204, 1, 99), Seq(204))
    testIO(Seq(109, 5, 203, 2, 4, 7, 99, 1), 0, Seq(123), Seq(123))
  }

  it should "handle puzzle quine" in {
    val program: Seq[Long] = Seq(109, 1, 204, -1, 1001, 100, 1, 100, 1008, 100, 16, 101, 1006, 101, 0, 99)
    testOutput(program, program)
  }

  it should "handle other examples" in {
    val program1: Seq[Long] = Seq(1102, 34915192, 34915192, 7, 4, 7, 99, 0)
    testOutput(program1, Seq(34915192L * 34915192L))

    val program2: Seq[Long] = Seq(104, 1125899906842624L, 99)
    testOutput(program2, Seq(1125899906842624L))
  }

  it should "run the puzzle in test mode" in new SetupPuzzleData {
    testIO(puzzleProgram, Int.MaxValue / 20, Seq(1), Seq(3335138414L))
  }

  it should "run the puzzle in boost mode" in new SetupPuzzleData {
    testIO(puzzleProgram, Int.MaxValue / 20, Seq(2), Seq(49122))
  }
}
