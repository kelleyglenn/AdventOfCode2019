package day16

class FFT {

  val basePattern: Seq[Short] = Seq(0, 1, 0, -1)

  def inputFromString(digits: String): Seq[Short] = {
    digits.toCharArray.map(_ - '0').map(_.toShort)
  }

  def stringFromInput(input: Seq[Short]): String = {
    input.map(_ + '0').map(_.toChar).mkString
  }

  def expandBasePatternToSize(size: Int, copies: Int): Seq[Short] = {
    val newPattern: Seq[Short] = basePattern.flatMap(Seq.fill(copies)(_))
    val expandedPattern: Seq[Short] = Seq.fill(Math.ceil(size.toDouble / newPattern.size).toInt)(newPattern).flatten
    expandedPattern.splitAt(size)._1
  }

  def executePhase(input: Seq[Short]): Seq[Short] = {
    (1 to input.size).map(i => expandPatternAndApplyToInput(input, i.toShort))
  }

  def expandPatternAndApplyToInput(input: Seq[Short], outputDigit: Int): Short = {
    val pattern = expandBasePatternToSize(input.size + 1, outputDigit)
    applyPattern(input, pattern.tail)
  }

  def executePhasesUpTo(input: Seq[Short], phaseNumber: Short): Seq[Short] = {
    assert(phaseNumber > 0)
    var curInput = input
    for (_ <- 1 to phaseNumber) {
      curInput = executePhase(curInput)
    }
    curInput
  }

  def applyPattern(input: Seq[Short], pattern: Seq[Short]): Short = {
    assert(input.size == pattern.size)
    (Math.abs(input.zip(pattern).map(x => x._1 * x._2).sum) % 10).toShort
  }
}
