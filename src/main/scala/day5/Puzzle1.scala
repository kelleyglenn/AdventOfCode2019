package day5

object Puzzle1 {


  def process(program: Array[Int], currIdx: Int, inputs: Seq[Int] = Seq.empty, runningOut: Seq[Int] = Seq.empty):
  (Array[Int], Seq[Int]) = {
    val op: Int = program(currIdx) % 100
    val mode: Int = program(currIdx) / 100

    def getIndexes(): (Int, Int) = {
      val mode1Immediate: Boolean = mode % 10 == 1
      val mode2Immediate: Boolean = (mode / 10) % 10 == 1
      val srcIdx1: Int = if (mode1Immediate) currIdx + 1 else program(currIdx + 1)
      val srcIdx2: Int = if (mode2Immediate) currIdx + 2 else program(currIdx + 2)
      (srcIdx1, srcIdx2)
    }

    if (op == 1 || op == 2 || op == 7 || op == 8) {
      val (srcIdx1, srcIdx2) = getIndexes()
      val tgtIdx: Int = program(currIdx + 3)
      val val1: Int = program(srcIdx1)
      val val2: Int = program(srcIdx2)
      val ans: Int = if (op == 1) val1 + val2 else if (op == 2) val1 * val2 else if (op == 7) (if (val1 < val2) 1 else 0) else
        (if (val1 == val2) 1 else 0)
      program(tgtIdx) = ans
      process(program, currIdx + 4, inputs, runningOut)
    } else if (op == 3) { // input
      if (inputs.isEmpty) throw new RuntimeException("No input found")
      val tgtIdx: Int = program(currIdx + 1)
      program(tgtIdx) = inputs.head
      process(program, currIdx + 2, inputs.tail, runningOut)
    } else if (op == 4) { // output
      val modeImmediate: Boolean = mode % 10 == 1
      val srcIdx: Int = if (modeImmediate) currIdx + 1 else program(currIdx + 1)
      process(program, currIdx + 2, inputs, runningOut :+ program(srcIdx))
    } else if (op == 5) { // jump-if-true
      val (srcIdx1, srcIdx2) = getIndexes()
      val newIdx = if (program(srcIdx1) != 0) program(srcIdx2) else currIdx + 3
      process(program, newIdx, inputs, runningOut)
    } else if (op == 6) { // jump-if-false
      val (srcIdx1, srcIdx2) = getIndexes()
      val newIdx = if (program(srcIdx1) == 0) program(srcIdx2) else currIdx + 3
      process(program, newIdx, inputs, runningOut)
    } else if (op == 99) {
      (program, runningOut)
    } else throw new RuntimeException("Invalid op: " + op)
  }
}
