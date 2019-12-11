package day11

import java.util.concurrent.BlockingQueue

object Puzzle1 {

  abstract class Direction(val xDiff: Short, val yDiff: Short) {
    def turn90(clockwise: Boolean): Direction
  }

  object U extends Direction(0, 1) {
    override def turn90(clockwise: Boolean): Direction = if (clockwise) R else L
  }

  object L extends Direction(-1, 0) {
    override def turn90(clockwise: Boolean): Direction = if (clockwise) U else D
  }

  object D extends Direction(0, -1) {
    override def turn90(clockwise: Boolean): Direction = U.turn90(!clockwise)
  }

  object R extends Direction(1, 0) {
    override def turn90(clockwise: Boolean): Direction = L.turn90(!clockwise)
  }

  class Robot( instructions: BlockingQueue[Long], cameraOutput: BlockingQueue[Long], start: Char = '.') extends Thread("Robot") {
    val black: Char = '.'
    val white: Char = '#'

    val colorCharToShort: Map[Char, Short] = Seq('.', '#').zipWithIndex.map(x => (x._1, x._2.toShort)).toMap
    val colorShortToChar: Map[Short, Char] = colorCharToShort.toSeq.map(_.swap).toMap
    var paintedPos: Map[(Int, Int), Char] = Map((0, 0) -> start)
    var curPos: (Int, Int) = (0, 0)
    var curDirection: Direction = U
    var shouldEnd: Boolean = false

    def numPaintedPos(): Int = paintedPos.keySet.size

    def getNextShort: Short = {
      instructions.take().toShort
    }

    def getColor: Short = getNextShort

    def getDirection: Boolean = getNextShort == 1

    override def run(): Unit = {
      def handleOnePair(): Unit = {
        val curColor = paintedPos.getOrElse(curPos, black)
        cameraOutput.put(colorCharToShort(curColor))
        paintedPos = paintedPos + (curPos -> colorShortToChar(getColor))
        curDirection = curDirection.turn90(getDirection)
        curPos = (curPos._1 + curDirection.xDiff, curPos._2 + curDirection.yDiff)
      }

      while (!shouldEnd) {
        handleOnePair()
      }
    }
  }

}
