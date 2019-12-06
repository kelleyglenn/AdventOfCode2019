package day2

object Puzzle1 {
  @scala.annotation.tailrec
  def process(program: Array[Int], currIdx: Int): Array[Int] = {
    val op: Int = program(currIdx)
    if (op == 1 || op == 2) {
      val srcIdx1: Int = program(currIdx + 1)
      val srcIdx2: Int = program(currIdx + 2)
      val tgtIdx: Int = program(currIdx + 3)
      val val1: Int = program(srcIdx1)
      val val2: Int = program(srcIdx2)
      val ans: Int = if (op == 1) val1 + val2 else val1 * val2
      program(tgtIdx) = ans
      process(program, currIdx + 4)
    } else if (op == 99) {
      program
    } else throw new RuntimeException("Invalid op: " + op)
  }
}
