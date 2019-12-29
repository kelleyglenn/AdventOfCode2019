package day13

import java.util.concurrent.LinkedBlockingQueue

import day9.Puzzle1.Computer
import org.scalatest.flatspec.AnyFlatSpec

import scala.collection.mutable.ArrayBuffer
import scala.io.{BufferedSource, Source}

class ArcadeCabinetTest extends AnyFlatSpec {

  class SetupPuzzleData(name: String) {
    val bufferedSource: BufferedSource =
      Source.fromURL(getClass.getResource("/day13/" + name + ".txt"))
    val puzzleString: String = bufferedSource.getLines.toSeq.head
    val puzzleProgram: Seq[Long] = puzzleString.split(',').map(_.toLong).toSeq
    bufferedSource.close()
  }

  behavior of "logic"
  it should "solve the first puzzle" in new SetupPuzzleData("input") {
    val memory: ArrayBuffer[Long] = {
      val m: ArrayBuffer[Long] = ArrayBuffer.from(puzzleProgram.toArray)
      m.addAll(new Array[Long](1000))
    }
    val c: Computer =
      new Computer(memory.toArray, null, new LinkedBlockingQueue[Long]())
    c.start()
    c.join()
    var results: Map[(Long, Long), Long] = Map.empty
    while (!c.output.isEmpty) {
      results += ((c.output.take(), c.output.take()) -> c.output.take())
    }
    assert(results.values.count(_ == 2) == 258)
  }

//  it should "solve the second puzzle" in new SetupPuzzleData("input") {
//    val cab = new ArcadeCabinet(puzzleProgram)
//    assert(cab.play == 1)
//  }

}
