package day7

import java.util.concurrent.{BlockingQueue, LinkedBlockingQueue}

object Puzzle2 {

  class Computer(val memory: Array[Int], val input: BlockingQueue[Int] = null, val output: BlockingQueue[Int] = null,
                 val startingPC: Int = 0) extends Thread {
    private var endReached: Boolean = false

    abstract class Op(val id: Int) {
      def doExec(mode: Int): Int

      def exec(mode: Int): Unit = {
        pc = doExec(mode)
      }

      def getIndex(mode: Int, pcOffset: Int): Int = {
        if (mode == 1) pc + pcOffset else memory(pc + pcOffset)
      }

      def getIndexes(mode: Int): (Int, Int) = {
        (getIndex(mode % 10, 1), getIndex((mode / 10) % 10, 2))
      }
    }

    abstract class MathOp(override val id: Int, val calc: (Int, Int) => Int) extends Op(id) {
      override def doExec(mode: Int): Int = {
        val (srcIdx1, srcIdx2) = getIndexes(mode)
        val tgtIdx: Int = memory(pc + 3)
        val val1: Int = memory(srcIdx1)
        val val2: Int = memory(srcIdx2)
        memory(tgtIdx) = calc(val1, val2)
        pc + 4
      }
    }

    class JumpOp(override val id: Int, val f: Int => Boolean) extends Op(id) {
      override def doExec(mode: Int): Int = {
        val (srcIdx1, srcIdx2) = getIndexes(mode)
        if (f(memory(srcIdx1))) memory(srcIdx2) else pc + 3
      }
    }

    object AddOp extends MathOp(1, (val1: Int, val2: Int) => val1 + val2)

    object MultiplyOp extends MathOp(2, (val1: Int, val2: Int) => val1 * val2)

    object InputOp extends Op(3) {
      override def doExec(mode: Int): Int = {
        val tgtIdx: Int = memory(pc + 1)
        memory(tgtIdx) = input.take()
        pc + 2
      }
    }

    object OutputOp extends Op(4) {
      override def doExec(mode: Int): Int = {
        output.put(memory(getIndex(mode % 10, 1)))
        pc + 2
      }
    }

    object JumpIfTrue extends JumpOp(5, (val1: Int) => val1 != 0)

    object JumpIfFalse extends JumpOp(6, (val1: Int) => val1 == 0)

    object LessThanOp extends MathOp(7, (val1: Int, val2: Int) => if (val1 < val2) 1 else 0)

    object EqualsOp extends MathOp(8, (val1: Int, val2: Int) => if (val1 == val2) 1 else 0)

    object EndOp extends Op(99) {
      override def doExec(mode: Int): Int = {
        endReached = true
        pc + 0
      }
    }

    private val opMap: Map[Int, Op] = Set(AddOp, MultiplyOp, InputOp, OutputOp, JumpIfTrue, JumpIfFalse,
      LessThanOp, EqualsOp, EndOp).map((op: Op) => (op.id, op)).toMap

    private var pc: Int = startingPC

    override def run(): Unit = {
      while (!endReached) {
        val op: Int = memory(pc) % 100
        val mode: Int = memory(pc) / 100
        opMap(op).exec(mode)
      }
    }
  }

  def processWithFeedback(originalProgram: Seq[Int], settings: Seq[Int]): Int = {
    val inputQueues: Seq[BlockingQueue[Int]] = settings.map(_ => new LinkedBlockingQueue[Int]())
    val outputQueues: Seq[BlockingQueue[Int]] = inputQueues.tail :+ inputQueues.head
    val computers: Seq[Computer] =
      inputQueues.zip(outputQueues).map { case (in, out) => new Computer(originalProgram.toArray, in, out) }
    val io = inputQueues.head
    inputQueues.zip(settings).foreach { case (q, setting) => q.put(setting) }
    computers.foreach(_.start())
    io.put(0)
    computers.foreach(_.join())
    io.take()
  }

  def findSettingsWithHighestOutput(program: Seq[Int]): Int = {
    (for (a <- 5 to 9; b <- 5 to 9; c <- 5 to 9; d <- 5 to 9; e <- 5 to 9)
      yield {
        val settings: Seq[Int] = Seq(a, b, c, d, e)
        if (settings.distinct.size == settings.size)
          Some(processWithFeedback(program, settings))
        else None
      }).flatten.max
  }
}
