package day1

import org.scalatest.flatspec.AnyFlatSpec

class Puzzle1Test extends AnyFlatSpec {
  behavior of "calculateRequiredFuel"
  it should "handle single values" in {
    assert(Puzzle1.calculateRequiredFuel(12) == 2)
    assert(Puzzle1.calculateRequiredFuel(14) == 2)
    assert(Puzzle1.calculateRequiredFuel(1969) == 654)
    assert(Puzzle1.calculateRequiredFuel(100756) == 33583)
  }

  it should "handle a collection of values" in {
    assert(Puzzle1.calculateRequiredFuel(Seq(12)) == (2))
    assert(Puzzle1.calculateRequiredFuel(Seq(12, 14)) == (2 + 2))
    assert(Puzzle1.calculateRequiredFuel(Seq(12, 14, 1969, 100756)) == (2 + 2 + 654 + 33583))
  }

  it should "answer the puzzle" in {
    val input: Seq[Int] = Seq(109024,137172,80445,80044,107913,108989,59638,120780,139262,139395,56534,129398,101732,101212,142352,123971,75207,121384,145719,66925,71782,102129,83220,147045,99092,132909,114504,141549,99552,128422,134505,58295,142325,107896,66181,86080,71393,58839,143851,149540,108206,68353,123196,61256,83896,122756,133066,138085,129872,63965,105520,141513,90305,92651,113284,66895,72068,144011,82963,136919,111222,54134,82662,107612,87366,131791,144708,116894,142784,52299,138414,56330,80146,73422,60308,125678,95910,116374,136257,100387,114960,62651,102946,56912,91399,146005,147222,125962,129805,101208,67998,85297,117332,101967,94339,130878,79396,140312,147746,136975)
    assert(input.size == 100)
    assert(Puzzle1.calculateRequiredFuel(input) == 3520097)
  }
}
