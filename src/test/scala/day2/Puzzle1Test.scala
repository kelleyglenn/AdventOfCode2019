package day2

import org.scalatest.flatspec.AnyFlatSpec

class Puzzle1Test extends AnyFlatSpec {
  behavior of "process"
  it should "handle no op" in {
    val program = Array(99)
    assert(Puzzle1.process(program, 0) sameElements program)
  }

  it should "handle simple addition" in {
    test(Array(1, 0, 0, 0, 99), Array(2, 0, 0, 0, 99))
  }

  it should "handle simple multiplication" in {
    test(Array(2, 3, 0, 3, 99), Array(2, 3, 0, 6, 99))
  }

  it should "handle storage after end" in {
    test(Array(2, 4, 4, 5, 99, 0), Array(2, 4, 4, 5, 99, 9801))
  }

  it should "handle modified opcode" in {
    test(Array(1, 1, 1, 4, 99, 5, 6, 0, 99), Array(30, 1, 1, 4, 2, 5, 6, 0, 99))
  }

  it should "answer the puzzle" in {
    val program: Array[Int] = Array(1, 12, 2, 3, 1, 1, 2, 3, 1, 3, 4, 3, 1, 5, 0, 3, 2, 6, 1, 19, 1, 5, 19, 23, 2, 6, 23, 27, 1, 27, 5, 31, 2, 9, 31, 35, 1, 5, 35, 39, 2, 6, 39, 43, 2, 6, 43, 47, 1, 5, 47, 51, 2, 9, 51, 55, 1, 5, 55, 59, 1, 10, 59, 63, 1, 63, 6, 67, 1, 9, 67, 71, 1, 71, 6, 75, 1, 75, 13, 79, 2, 79, 13, 83, 2, 9, 83, 87, 1, 87, 5, 91, 1, 9, 91, 95, 2, 10, 95, 99, 1, 5, 99, 103, 1, 103, 9, 107, 1, 13, 107, 111, 2, 111, 10, 115, 1, 115, 5, 119, 2, 13, 119, 123, 1, 9, 123, 127, 1, 5, 127, 131, 2, 131, 6, 135, 1, 135, 5, 139, 1, 139, 6, 143, 1, 143, 6, 147, 1, 2, 147, 151, 1, 151, 5, 0, 99, 2, 14, 0, 0)
    assert(Puzzle1.process(program, 0).head == 4484226)
  }

  it should "find inputs for a known total" in {
    val program: Array[Int] = Array(1, 0, 0, 3, 1, 1, 2, 3, 1, 3, 4, 3, 1, 5, 0, 3, 2, 6, 1, 19, 1, 5, 19, 23, 2, 6, 23, 27, 1, 27, 5, 31, 2, 9, 31, 35, 1, 5, 35, 39, 2, 6, 39, 43, 2, 6, 43, 47, 1, 5, 47, 51, 2, 9, 51, 55, 1, 5, 55, 59, 1, 10, 59, 63, 1, 63, 6, 67, 1, 9, 67, 71, 1, 71, 6, 75, 1, 75, 13, 79, 2, 79, 13, 83, 2, 9, 83, 87, 1, 87, 5, 91, 1, 9, 91, 95, 2, 10, 95, 99, 1, 5, 99, 103, 1, 103, 9, 107, 1, 13, 107, 111, 2, 111, 10, 115, 1, 115, 5, 119, 2, 13, 119, 123, 1, 9, 123, 127, 1, 5, 127, 131, 2, 131, 6, 135, 1, 135, 5, 139, 1, 139, 6, 143, 1, 143, 6, 147, 1, 2, 147, 151, 1, 151, 5, 0, 99, 2, 14, 0, 0)
    assert(findInputsForTotal(program, 4484226) == Some(12, 2))
  }

  it should "find inputs for the puzzle" in {
    val program: Array[Int] = Array(1, 0, 0, 3, 1, 1, 2, 3, 1, 3, 4, 3, 1, 5, 0, 3, 2, 6, 1, 19, 1, 5, 19, 23, 2, 6, 23, 27, 1, 27, 5, 31, 2, 9, 31, 35, 1, 5, 35, 39, 2, 6, 39, 43, 2, 6, 43, 47, 1, 5, 47, 51, 2, 9, 51, 55, 1, 5, 55, 59, 1, 10, 59, 63, 1, 63, 6, 67, 1, 9, 67, 71, 1, 71, 6, 75, 1, 75, 13, 79, 2, 79, 13, 83, 2, 9, 83, 87, 1, 87, 5, 91, 1, 9, 91, 95, 2, 10, 95, 99, 1, 5, 99, 103, 1, 103, 9, 107, 1, 13, 107, 111, 2, 111, 10, 115, 1, 115, 5, 119, 2, 13, 119, 123, 1, 9, 123, 127, 1, 5, 127, 131, 2, 131, 6, 135, 1, 135, 5, 139, 1, 139, 6, 143, 1, 143, 6, 147, 1, 2, 147, 151, 1, 151, 5, 0, 99, 2, 14, 0, 0)
    assert(findInputsForTotal(program, 19690720) == Some(56, 96))
  }

  def test(source: Array[Int], expected: Array[Int]): Unit = {
    assert(Puzzle1.process(source, 0).toIndexedSeq == expected.toIndexedSeq)
  }

  def findInputsForTotal(origProgram: Array[Int], total: Int): Option[(Int, Int)] = {
    val totalToInputs: Map[Int, (Int, Int)] =
      (for (noun <- 0 to 99; verb <- 0 to 99) yield createMapEntryFromProgram(origProgram, noun, verb)).toMap
    assert(totalToInputs.size == 10000)
    totalToInputs.get(total)
  }

  def createMapEntryFromProgram(origProgram: Array[Int], noun: Int, verb: Int): (Int, (Int, Int)) = {
    val currProgram: Array[Int] = origProgram.clone()
    currProgram(1) = noun
    currProgram(2) = verb
    Puzzle1.process(currProgram, 0).head -> (noun, verb)
  }
}
