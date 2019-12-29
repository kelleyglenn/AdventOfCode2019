package day13

import java.util
import java.util.concurrent.{BlockingQueue, LinkedBlockingQueue, TimeUnit}

import day9.Puzzle1.Computer

import scala.collection.mutable.ArrayBuffer

class SingleValueBlockingQueue[T](start: T)
    extends util.AbstractQueue[T]
    with BlockingQueue[T] {

  var value: T = start

  override def offer(e: T): Boolean = this.synchronized {
    put(e)
    true
  }

  override def put(e: T): Unit = this.synchronized {
    value = e
  }

  override def offer(e: T, timeout: Long, unit: TimeUnit): Boolean = offer(e)

  override def take(): T = this.synchronized {
    value
  }

  override def poll(timeout: Long, unit: TimeUnit): T = take()

  override def remainingCapacity(): Int = 1

  override def drainTo(c: util.Collection[_ >: T]): Int = synchronized {
    c.add(value)
    1
  }

  override def drainTo(c: util.Collection[_ >: T], maxElements: Int): Int =
    drainTo(c)

  override def poll(): T = take()

  override def peek(): T = take()

  override def iterator(): util.Iterator[T] = new util.Iterator[T] {
    override def hasNext: Boolean = true

    override def next(): T = SingleValueBlockingQueue.this.synchronized {
      take()
    }
  }

  override def size(): Int = 1
}

class ArcadeCabinet(program: Seq[Long]) {
  val memory: ArrayBuffer[Long] = {
    val m: ArrayBuffer[Long] = ArrayBuffer.from(program.toArray)
    m.addAll(new Array[Long](1000))
  }

  def play: Long = {
    var paddlePos: Option[(Long, Long)] = None
    var ballPos: Option[(Long, Long)] = None
    var jsDir: Option[Long] = None
    var tgtPaddleX: Long = -1

    def processOutput(joystick: BlockingQueue[Long],
                      screen: BlockingQueue[Long],
                      latestScore: Long): Long = {
      def movePaddle(tgt: Long, paddle: Option[(Long, Long)]): Unit = {
        if (paddle.nonEmpty) {
          val move: Int = if (tgt >= 0) tgt.compareTo(paddle.get._1) else 0
//          if (jsDir.isEmpty || jsDir.get != move) {
          println("Moving paddle " + move)
          jsDir = Some(move)
          joystick.put(move)
//          }
        }
      }

      val x: Long = screen.take()
      val y: Long = screen.take()
      val tileType: Long = screen.take()
      if (tileType == 3 || tileType == 4) {
        println(
          (if (tileType == 3) "paddle" else "  ball") + "=" + x + "," + y + "," + tileType
        )
      }
      if (x == -1 && y == 0) {
        println("Score=" + tileType)
        tileType // new score
      } else {
        tileType match {
          case 3 =>
            paddlePos = Some(x, y)
            println("case 3 needed X=" + tgtPaddleX)
            movePaddle(tgtPaddleX, paddlePos)
          case 4 =>
            if (ballPos.nonEmpty && paddlePos.nonEmpty && ballPos.get._2 < y) {
              tgtPaddleX = ((x - ballPos.get._1) * (paddlePos.get._2 - y)) + x
              println("case 4 needed X=" + tgtPaddleX)
            }
            ballPos = Some(x, y)
            movePaddle(tgtPaddleX, paddlePos)
          case _ =>
        }
        latestScore
      }
    }

    insertQuarter(2)
    val c: Computer = new Computer(
      memory.toArray,
      new LinkedBlockingQueue[Long](),
      new LinkedBlockingQueue[Long]()
    )
    c.start()
    var latestScore: Long = 0
    while (!c.endReached) {
      if (!c.output.isEmpty) {
        latestScore = processOutput(c.input, c.output, latestScore)
      }
      Thread.sleep(10)
    }
    c.join()
    println("Computer done")
    while (!c.output.isEmpty) {
      latestScore = processOutput(c.input, c.output, latestScore)
    }
    latestScore
  }

  def insertQuarter(quarterCt: Int): Unit = memory(0) = quarterCt

}
