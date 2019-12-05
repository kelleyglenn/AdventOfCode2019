package day1

object Puzzle1 {
  def calculateRequiredFuel(mass: Int): Int = {
    (mass / 3) - 2
  }

  def calculateRequiredFuel(masses: Seq[Int]): Int = {
    val indFuel: Seq[Int] = masses.map(calculateRequiredFuel(_))
    val ans: Int = indFuel.fold(0)((a: Int, b: Int) => a + b)
    ans
  }
}
