package day4

import org.scalatest.flatspec.AnyFlatSpec

class Puzzle1Test extends AnyFlatSpec {
  behavior of "meetsCriteria"
  it should "handle length " in {
    assert(Puzzle1.meetsCriteria(113456))
    assert(!Puzzle1.meetsCriteria(11345))
    assert(!Puzzle1.meetsCriteria(1134567))
  }

  it should "handle decreasing" in {
    assert(Puzzle1.meetsCriteria(113456))
    assert(!Puzzle1.meetsCriteria(651234))
    assert(!Puzzle1.meetsCriteria(123465))
  }

  it should "handle doubles" in {
    assert(Puzzle1.meetsCriteria(113456))
    assert(!Puzzle1.meetsCriteria(123456))
    assert(!Puzzle1.meetsCriteria(456789))
  }

  it should "handle puzzle examples" in {
    assert(Puzzle1.meetsCriteria(111111))
    assert(!Puzzle1.meetsCriteria(223450))
    assert(!Puzzle1.meetsCriteria(123789))
  }

  it should "solve the puzzle" in {
    assert((172851 to 675869).count(Puzzle1.meetsCriteria(_)) == 1660)
  }

  behavior of "meetsCriteria2"
  it should "handle doubles" in {
    assert(Puzzle1.meetsCriteria2(113456))
    assert(!Puzzle1.meetsCriteria2(111456))
    assert(!Puzzle1.meetsCriteria2(123456))
    assert(!Puzzle1.meetsCriteria2(456789))
  }

  it should "handle puzzle examples" in {
    assert(Puzzle1.meetsCriteria2(112233))
    assert(!Puzzle1.meetsCriteria2(123444))
    assert(Puzzle1.meetsCriteria2(111122))
  }

  it should "solve the puzzle" in {
    assert((172851 to 675869).count(Puzzle1.meetsCriteria2(_)) == 1135)
  }
}
