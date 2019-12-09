package day9

import java.util.concurrent.BlockingQueue

object Puzzle1 {

  class Computer(val memory: Array[Long], val input: BlockingQueue[Long] = null, val output: BlockingQueue[Long] = null,
                 val startingPC: Int = 0) extends Thread {
    private var endReached: Boolean = false
    private var pc: Int = startingPC
    private var relativeBase: Int = 0

    abstract class Op(val id: Short) {
      def doExec(mode: Short): Int

      def exec(mode: Short): Unit = {
        pc = doExec(mode)
      }

      def getIndex(mode: Int, pcOffset: Int): Int = {
        if (mode == 1) pc + pcOffset else memory(pc + pcOffset).toInt + (if (mode == 2) relativeBase else 0)
      }

      def getIndexes(mode: Int): (Int, Int, Int) = {
        (getIndex(mode % 10, 1), getIndex((mode / 10) % 10, 2), getIndex((mode / 100) % 10, 3))
      }
    }

    abstract class MathOp(override val id: Short, val calc: (Long, Long) => Long) extends Op(id) {
      override def doExec(mode: Short): Int = {
        val (srcIdx1, srcIdx2, tgtIdx) = getIndexes(mode)
        val val1: Long = memory(srcIdx1)
        val val2: Long = memory(srcIdx2)
        memory(tgtIdx) = calc(val1, val2)
        pc + 4
      }
    }

    class JumpOp(override val id: Short, val f: Long => Boolean) extends Op(id) {
      override def doExec(mode: Short): Int = {
        val (srcIdx1, srcIdx2, _) = getIndexes(mode)
        if (f(memory(srcIdx1))) memory(srcIdx2).toInt else pc + 3
      }
    }

    object AddOp extends MathOp(1, (val1: Long, val2: Long) => val1 + val2)

    object MultiplyOp extends MathOp(2, (val1: Long, val2: Long) => val1 * val2)

    object InputOp extends Op(3) {
      override def doExec(mode: Short): Int = {
        val tgtIdx: Int = getIndex(mode, 1)
        memory(tgtIdx) = input.take()
        pc + 2
      }
    }

    object OutputOp extends Op(4) {
      override def doExec(mode: Short): Int = {
        val srcIdx = getIndex(mode, 1)
        output.put(memory(srcIdx))
        pc + 2
      }
    }

    object JumpIfTrue extends JumpOp(5, (val1: Long) => val1 != 0)

    object JumpIfFalse extends JumpOp(6, (val1: Long) => val1 == 0)

    object LessThanOp extends MathOp(7, (val1: Long, val2: Long) => if (val1 < val2) 1 else 0)

    object EqualsOp extends MathOp(8, (val1: Long, val2: Long) => if (val1 == val2) 1 else 0)

    object AdjustRelativeBaseOp extends Op(9) {
      override def doExec(mode: Short): Int = {
        val tgtIdx: Int = getIndex(mode, 1)
        relativeBase += memory(tgtIdx).toInt
        pc + 2
      }
    }

    object EndOp extends Op(99) {
      override def doExec(mode: Short): Int = {
        endReached = true
        pc + 0
      }
    }

    private val opMap: Map[Short, Op] = Set(AddOp, MultiplyOp, InputOp, OutputOp, JumpIfTrue, JumpIfFalse,
      LessThanOp, EqualsOp, AdjustRelativeBaseOp, EndOp).map((op: Op) => (op.id, op)).toMap

    def execNext(): Unit = {
      val opMode = memory(pc)
      val op = opMap((opMode % 100).toShort)
      val mode = (opMode / 100).toShort
      op.exec(mode)
    }

    override def run(): Unit = {
      while (!endReached) {
        execNext()
      }
    }
  }
}
