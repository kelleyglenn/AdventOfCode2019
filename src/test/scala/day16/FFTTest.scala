package day16

import org.scalatest.flatspec.AnyFlatSpec

class FFTTest extends AnyFlatSpec {

  class SampleData {
    val FFT = new FFT
    val example1: Seq[Short] = FFT.inputFromString("80871224585914546619083218645595")
    val expected1: Seq[Short] = FFT.inputFromString("24176176")
    val example2: Seq[Short] = FFT.inputFromString("19617804207202209144916044189917")
    val expected2: Seq[Short] = FFT.inputFromString("73745418")
    val example3: Seq[Short] = FFT.inputFromString("69317163492948606335995924319873")
    val expected3: Seq[Short] = FFT.inputFromString("52432133")
  }

  class SampleData2 {
    val FFT = new FFT
    val example1: Seq[Short] = FFT.inputFromString("03036732577212944063491565474664")
    val expected1: Seq[Short] = FFT.inputFromString("84462026")
    val example2: Seq[Short] = FFT.inputFromString("02935109699940807407585447034323")
    val expected2: Seq[Short] = FFT.inputFromString("78725270")
    val example3: Seq[Short] = FFT.inputFromString("03081770884921959731165446850517")
    val expected3: Seq[Short] = FFT.inputFromString("53553731")
  }

  class PuzzleData {
    val FFT = new FFT
    val input: Seq[Short] = FFT.inputFromString("59773419794631560412886746550049210714854107066028081032096591759575145680294995770741204955183395640103527371801225795364363411455113236683168088750631442993123053909358252440339859092431844641600092736006758954422097244486920945182483159023820538645717611051770509314159895220529097322723261391627686997403783043710213655074108451646685558064317469095295303320622883691266307865809481566214524686422834824930414730886697237161697731339757655485312568793531202988525963494119232351266908405705634244498096660057021101738706453735025060225814133166491989584616948876879383198021336484629381888934600383957019607807995278899293254143523702000576897358")
    val expected1: Seq[Short] = FFT.inputFromString("12541048")
    val expected2: Seq[Short] = FFT.inputFromString("12541048")
  }

  behavior of "inputFromString"
  it should "handle any string digits" in new SampleData {
    assert(FFT.inputFromString("8348991") == Seq(8, 3, 4, 8, 9, 9, 1))
  }

  behavior of "stringFromInput"
  it should "handle any string digits" in new SampleData {
    assert(FFT.stringFromInput(Seq(8, 3, 4, 8, 9, 9, 1)) == "8348991")
  }

  behavior of "applyPattern"
  it should "handle first example" in new SampleData {
    assert(FFT.applyPattern(Seq(9, 8, 7, 6, 5), Seq(1, 2, 3, 1, 2)) == 2)
  }

  behavior of "expandBasePatternToSize"
  it should "handle common cases" in new SampleData {
    assert(FFT.expandBasePatternToSize(FFT.basePattern.size.toShort, 1) == FFT.basePattern)
    assert(FFT.expandBasePatternToSize((FFT.basePattern.size + 1).toShort, 1) == FFT.basePattern :+ FFT.basePattern.head)
    assert(FFT.expandBasePatternToSize((FFT.basePattern.size + 1).toShort, 2) == Seq(0, 0, 1, 1, 0))
    assert(FFT.expandBasePatternToSize(5, 2) == Seq(0, 0, 1, 1, 0))
    assert(FFT.expandBasePatternToSize(16, 3) == Seq(0, 0, 0, 1, 1, 1, 0, 0, 0, -1, -1, -1, 0, 0, 0, 1))
  }

  behavior of "executePhase"
  it should "handle example cases" in new SampleData {
    assert(FFT.executePhase(FFT.inputFromString("12345678")) == FFT.inputFromString("48226158"))
    assert(FFT.executePhase(FFT.inputFromString("48226158")) == FFT.inputFromString("34040438"))
    assert(FFT.executePhase(FFT.inputFromString("34040438")) == FFT.inputFromString("03415518"))
    assert(FFT.executePhase(FFT.inputFromString("03415518")) == FFT.inputFromString("01029498"))
  }

  behavior of "executePhasesUpTo"
  it should "handle example cases" in new SampleData {
    assert(FFT.executePhasesUpTo(FFT.inputFromString("12345678"), 1) == FFT.inputFromString("48226158"))
    assert(FFT.executePhasesUpTo(FFT.inputFromString("12345678"), 2) == FFT.inputFromString("34040438"))
    assert(FFT.executePhasesUpTo(FFT.inputFromString("12345678"), 3) == FFT.inputFromString("03415518"))
    assert(FFT.executePhasesUpTo(FFT.inputFromString("12345678"), 4) == FFT.inputFromString("01029498"))
  }

  it should "handle larger examples" in new SampleData {
    assert(FFT.executePhasesUpTo(example1, 100).splitAt(8)._1 == expected1)
    assert(FFT.executePhasesUpTo(example2, 100).splitAt(8)._1 == expected2)
    assert(FFT.executePhasesUpTo(example3, 100).splitAt(8)._1 == expected3)
  }

  it should "solve the first puzzle" in new PuzzleData {
    assert(FFT.executePhasesUpTo(input, 100).splitAt(8)._1 == expected1)
  }

  //  it should "handle the part two examples" in new SampleData2 {
  //      assert(FFT.executePhasesUpTo(Seq.fill(10000)(example1).flatten, 100).splitAt(303673 + 8)._1.splitAt(303673)._2 == expected1)
  //  }

  //  it should "solve the second puzzle" in new PuzzleData {
  //    val expandedInput: Seq[Short] = Seq.fill(10000)(input).flatten
  //    val answer: Seq[Short] = FFT.executePhasesUpTo(expandedInput, 100)
  //    println("Answer: " + FFT.stringFromInput(answer))
  //    assert(answer.splitAt(5977341 + 8)._1.splitAt(5977341)._2 == expected2)
  //  }
}
