package day7

object Puzzle1 {
  private val computer: day5.Puzzle1.type = day5.Puzzle1

  def processWithSettings(originalProgram: Seq[Int], settings: Seq[Int]): Int = {
    var nextInput: Int = 0
    for (setting <- settings) {
      nextInput = computer.process(originalProgram.toArray, 0, Seq(setting, nextInput))._2.head
    }
    nextInput
  }

  def findSettingsWithHighestOutput(program: Seq[Int]): Int = {
    (for (a <- 0 to 4; b <- 0 to 4; c <- 0 to 4; d <- 0 to 4; e <- 0 to 4)
      yield {
        val settings: Seq[Int] = Seq(a, b, c, d, e)
        if (settings.distinct.size == 5)
          Some(processWithSettings(program, settings))
        else None
      }).flatten.max
  }
}
