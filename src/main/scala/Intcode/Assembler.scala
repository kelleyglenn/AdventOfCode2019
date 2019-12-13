package Intcode

import scala.util.matching.Regex
import scala.util.matching.Regex.MatchIterator

object Assembler {

  class ParseException(t: Exception, line: Int) extends Exception("line(" + line + ")", t)

  class Op(val id: Int) {
    def validate(rawOpLines: RawOpLine): Unit = {}
  }

  class OpWithOutputParam(id: Int, outputPos: Int) extends Op(id) {
    override def validate(rawOpLine: RawOpLine): Unit = {
      if (rawOpLine.params(outputPos).mode == Mode.Immediate)
        throw new IllegalArgumentException("Parameter " + (outputPos + 1) + " cannot be Immediate mode")
    }
  }

  object NOP0 extends Op(0)

  object NOP1 extends Op(0)

  object NOP2 extends Op(0)

  object NOP3 extends Op(0)

  object ADD extends OpWithOutputParam(1, 2)

  object MULT extends OpWithOutputParam(2, 2)

  object IN extends OpWithOutputParam(3, 0)

  object OUT extends Op(4)

  object JNEZ extends Op(5)

  object JEQZ extends Op(6)

  object LESS extends OpWithOutputParam(7, 2)

  object EQ extends OpWithOutputParam(8, 2)

  object INCB extends Op(9)

  object END extends Op(99)

  val opLookup: Map[String, Op] = {
    val opSeq: Set[Op] = Set(NOP0, NOP1, NOP2, NOP3, ADD, MULT, IN, OUT, JNEZ, JEQZ, LESS, EQ, INCB, END)
    opSeq.map(op => (op.getClass.getSimpleName.dropRight(1), op)).toMap
  }

  object Mode extends Enumeration {
    type Mode = Value
    val Positional, Immediate, Relative = Value
  }

  case class RawParam(mode: Mode.Value, value: Either[Long, String])

  case class Param(mode: Mode.Value, value: Long)

  case class RawOpLine(op: Op, params: Seq[RawParam], startPosition: Int, nextPosition: Int)

  case class OpLine(op: Op, params: Seq[Param])

  val commentGroup: String = ";(.*)"
  val commentAfterGroup: String = raw"(?:\s+;(.*))?"
  val commentLineReg: Regex = commentGroup.r("comment")
  val bytePosGroup: String = raw"\s*(\d+)\:\s*"
  val createLabelGroup: String = raw"(?::(\w+))?"
  val paramGroup: String = raw"\s*[\s,]\s*(\@)?((?:(-?\d+)|([A-Za-z]\w+))(\+B)?)" + createLabelGroup
  val op0lineGroupReg: Regex =
    (bytePosGroup + "(NOP0|END)" + createLabelGroup + commentAfterGroup)
      .r("bytePos", "op", "opLabel", "comment")
  val op1lineGroupReg: Regex =
    (bytePosGroup + "(NOP1|IN|OUT|INCB)" + createLabelGroup + paramGroup + commentAfterGroup)
      .r("bytePos", "op", "opLabel",
        "positional", "param", "number", "referencedLabel", "relative", "createdLabel", "comment")
  val op2lineGroupReg: Regex =
    (bytePosGroup + "(NOP2|JNEZ|JEQZ)" + createLabelGroup + paramGroup + paramGroup + commentAfterGroup)
      .r("bytePos", "op", "opLabel",
        "positional1", "param1", "number1", "referencedLabel1", "relative1", "createdLabel1",
        "positional2", "param2", "number2", "referencedLabel2", "relative2", "createdLabel2", "comment")
  val op3lineGroupReg: Regex =
    (bytePosGroup + "(NOP3|ADD|MULT|LESS|EQ)" + createLabelGroup + paramGroup + paramGroup + paramGroup + commentAfterGroup)
      .r("bytePos", "op", "opLabel",
        "positional1", "param1", "number1", "referencedLabel1", "relative1", "createdLabel1",
        "positional2", "param2", "number2", "referencedLabel2", "relative2", "createdLabel2",
        "positional3", "param3", "number3", "referencedLabel3", "relative3", "createdLabel3", "comment")

  var labelMap: Map[String, Int] = Map.empty

  def rawParamWithSuffix(g: MatchIterator, suffix: String): RawParam = {
    val m: Mode.Value = {
      val positional: Boolean = g.group("positional" + suffix) != null
      val relative: Boolean = g.group("relative" + suffix) != null
      if (!positional && !relative) Mode.Immediate
      else if (positional && !relative) Mode.Positional
      else if (positional && relative) Mode.Relative
      else throw new IllegalArgumentException("Relative params must also be positional")
    }
    val value: Either[Long, String] = {
      val numericVal: String = g.group("number" + suffix)
      if (numericVal != null) Left(numericVal.toLong)
      else Right(g.group("referencedLabel" + suffix))
    }
    RawParam(m, value)
  }

  def addLabel(label: String, position: Int): Unit = {
    if (label != null) labelMap = labelMap + (label -> position)
  }

  def parseCommon(g: MatchIterator, position: Int): Unit = {
    if (g.group("bytePos").toInt != position) throw new IllegalArgumentException("Byte memoryPos should be " + position)
    addLabel(g.group("opLabel"), position)
  }

  def addLabels(g: MatchIterator, suffixes: Set[(String, Int)]): Unit = {
    suffixes.foreach(s => addLabel(g.group("createdLabel" + s._1), s._2))
  }

  def parseLine(line: String, memoryPos: Int): RawOpLine = {
    if (op0lineGroupReg.matches(line)) {
      val g: MatchIterator = op0lineGroupReg.findAllIn(line)
      parseCommon(g, memoryPos)
      RawOpLine(opLookup(g.group("op")), Seq.empty, memoryPos, memoryPos + 1)
    } else if (op1lineGroupReg.matches(line)) {
      val g: MatchIterator = op1lineGroupReg.findAllIn(line)
      parseCommon(g, memoryPos)
      val p: RawParam = rawParamWithSuffix(g, "")
      addLabels(g, Set(("", memoryPos + 1)))
      RawOpLine(opLookup(g.group("op")), Seq(p), memoryPos, memoryPos + 2)
    }
    else if (op2lineGroupReg.matches(line)) {
      val g: MatchIterator = op2lineGroupReg.findAllIn(line)
      parseCommon(g, memoryPos)
      val p1: RawParam = rawParamWithSuffix(g, "1")
      val p2: RawParam = rawParamWithSuffix(g, "2")
      addLabels(g, Set(("1", memoryPos + 1), ("2", memoryPos + 2)))
      RawOpLine(opLookup(g.group("op")), Seq(p1, p2), memoryPos, memoryPos + 3)
    }
    else if (op3lineGroupReg.matches(line)) {
      val g: MatchIterator = op3lineGroupReg.findAllIn(line)
      parseCommon(g, memoryPos)
      val p1: RawParam = rawParamWithSuffix(g, "1")
      val p2: RawParam = rawParamWithSuffix(g, "2")
      val p3: RawParam = rawParamWithSuffix(g, "3")
      addLabels(g, Set(("1", memoryPos + 1), ("2", memoryPos + 2), ("3", memoryPos + 3)))
      RawOpLine(opLookup(g.group("op")), Seq(p1, p2, p3), memoryPos, memoryPos + 4)
    }
    else throw new IllegalArgumentException("Unable to parseLine line: " + line)
  }

  def parseLines(lines: Seq[String], curMemoryPosition: Int = 0, lineNumber: Int = 0): Seq[RawOpLine] = {
    if (lines.isEmpty) Seq.empty
    else if (commentLineReg.matches(lines.head)) parseLines(lines.tail, curMemoryPosition)
    else {
      try {
        val newLine: RawOpLine = parseLine(lines.head, curMemoryPosition)
        newLine.op.validate(newLine)
        newLine +: parseLines(lines.tail, newLine.nextPosition, lineNumber + 1)
      }
      catch {
        case iae: IllegalArgumentException => throw new ParseException(iae, lineNumber)
      }
    }
  }

  def cookRawParam(rawParam: RawParam): Param = {
    Param(rawParam.mode, rawParam.value match {
      case Left(l) => l
      case Right(s) => if (labelMap.contains(s)) labelMap(s).toLong
      else throw new IllegalArgumentException("label '" + s + "' was used but not defined")
    })
  }

  def cookRawParams(params: Seq[RawParam]): Seq[Param] = {
    if (params.isEmpty) Seq.empty
    else cookRawParam(params.head) +: cookRawParams(params.tail)
  }

  def cookRawOpLine(line: RawOpLine): OpLine = {
    OpLine(line.op, cookRawParams(line.params))
  }

  def cookRawOpLines(rawOpLines: Seq[RawOpLine]): Seq[OpLine] = {
    if (rawOpLines.isEmpty) Seq.empty
    else cookRawOpLine(rawOpLines.head) +: cookRawOpLines(rawOpLines.tail)
  }

  def mode(param: Param): Short = {
    param.mode.id.toShort
  }

  def mode(params: Seq[Param]): Short = {
    if (params.isEmpty) 0
    else (mode(params.tail) * 10 + mode(params.head)).toShort
  }

  def serializeParams(params: Seq[Param]): Seq[Long] = {
    if (params.isEmpty) Seq.empty
    else params.head.value +: serializeParams(params.tail)
  }

  def serializeOpLine(opLine: OpLine): Seq[Long] = {
    (mode(opLine.params) * 100 + opLine.op.id).toLong +: serializeParams(opLine.params)
  }

  def serializeOpLines(opLines: Seq[OpLine]): Seq[Long] = {
    if (opLines.isEmpty) Seq.empty
    else serializeOpLine(opLines.head) ++ serializeOpLines(opLines.tail)
  }

  def assemble(lines: Seq[String]): Seq[Long] = {
    val rawOpLines: Seq[RawOpLine] = parseLines(lines)
    val opLines: Seq[OpLine] = cookRawOpLines(rawOpLines)
    serializeOpLines(opLines)
  }
}