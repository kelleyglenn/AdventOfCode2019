package day11

import java.util.concurrent.{BlockingQueue, LinkedBlockingQueue}

import day11.Puzzle1._
import day9.Puzzle1
import org.scalatest.flatspec.AnyFlatSpec

import scala.collection.mutable.ArrayBuffer
import scala.io.{BufferedSource, Source}
import scala.jdk.CollectionConverters._

class Puzzle1Test extends AnyFlatSpec {

  class SetupData {
    val robotInput: BlockingQueue[Long] = new LinkedBlockingQueue[Long]()
    val robotOutput: BlockingQueue[Long] = new LinkedBlockingQueue[Long]()
    val robot: Robot = new Robot(robotInput, robotOutput)
    val B = 0
    val W = 1
    val L = 0
    val R = 1
  }

  behavior of "robot"
  it should "handle manual example" in new SetupData {
    robot.start()
    assertStep(0, (W, L))
    assertStep(0, (B, L))
    robot.shouldEnd = true
    robot.join()
    assert(robot.paintedPos == Map((0, 0) -> robot.white, (-1, 0) -> robot.black))

    def assertStep(expected: Int, input: (Int, Int)): Unit = {
      assert(robotOutput.take() == expected)
      robotInput.addAll(Seq(input._1.toLong, input._2.toLong).asJava)
    }
  }

  it should "solve puzzle 1 with computer" in {
    val robot: Robot = createAndRunRobot()
    assert(robot.numPaintedPos() == 2293)
  }

  it should "solve puzzle 2 with computer" in {
    val robot: Robot = createAndRunRobot('#')
    for (y <- (40 to 55).reverse) {
      val scratch:Array[Char] = new Array[Char](60)
      for (x <- scratch.indices) {
        scratch(x) = robot.paintedPos.getOrElse((x - 10, y - 50), '.')
      }
      println(scratch.mkString)
    }
  }

  private def createAndRunRobot(start: Char = '.') = {
    val program: Array[Long] = {
      val bufferedSource: BufferedSource = Source.fromURL(getClass.getResource("/day11/input.txt"))
      val puzzleString: String = bufferedSource.getLines.toSeq.head
      val puzzleProgram: Seq[Long] = puzzleString.split(',').map(_.toLong).toSeq
      bufferedSource.close()
      val memory: ArrayBuffer[Long] = {
        val m: ArrayBuffer[Long] = ArrayBuffer.from(puzzleProgram.toArray)
        m.addAll(new Array[Long](10000))
      }
      memory.toArray
    }

    val computer = new Puzzle1.Computer(program, new LinkedBlockingQueue[Long](), new LinkedBlockingQueue[Long]())

    val robot: Robot = new Robot(computer.output, computer.input, start)
    computer.start()
    robot.start()
    computer.join()
    robot.shouldEnd = true
    robot
  }
}
