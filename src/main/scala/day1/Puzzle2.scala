package day1

object Puzzle2 {
  def calculateRequiredFuel(mass: Int): Int = {
    val fuel: Int = (mass / 3) - 2
    if (fuel <= 0) 0 else fuel + calculateRequiredFuel(fuel)
  }

  def calculateRequiredFuel(masses: Iterable[Int]): Int = {
    masses.map(calculateRequiredFuel).sum
  }
}
