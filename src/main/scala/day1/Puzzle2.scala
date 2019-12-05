package day1

object Puzzle2 {
  def calculateRequiredFuel(mass: Int): Int = {
    val fuel: Int = (mass / 3) - 2
    if (fuel <= 0) 0 else (fuel + calculateRequiredFuel(fuel))
  }

  def calculateRequiredFuel(masses: Seq[Int]): Int = {
    val indFuel: Seq[Int] = masses.map(calculateRequiredFuel(_))
    val ans: Int = indFuel.fold(0)((a: Int, b: Int) => a + b)
    ans
  }
}
