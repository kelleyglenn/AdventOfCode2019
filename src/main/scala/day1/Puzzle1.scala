package day1

object Puzzle1 {
  def calculateRequiredFuel(masses: Iterable[Int]): Int = {
    masses.map(i => (i / 3) - 2).sum
  }
}
