package Intcode

import Intcode.Assembler._
import org.scalatest.flatspec.AnyFlatSpec

import scala.util.matching.Regex

class AssemblerTest extends AnyFlatSpec {
  behavior of "Regex groups"
  it must "define bytePosGroup correctly" in {
    val regEx: Regex = bytePosGroup.r
    assert(regEx.matches("123:"))
    assert(regEx.matches(" 123:"))
    assert(regEx.matches(" 123:  "))
    assert(regEx.matches("\t\t123:\t"))
    assert(!regEx.matches("ABC"))
  }

  it must "define op0lineGroupReg correctly" in {
    assert(op0lineGroupReg.matches(" 1: NOP0"))
    assert(op0lineGroupReg.matches(" 1: NOP0:start ;start here"))
    assert(op0lineGroupReg.findAllIn("12: END").group("bytePos") == "12")
    assert(op0lineGroupReg.findAllIn("12: END").group("op") == "END")
    assert(op0lineGroupReg.findAllIn(" 1: NOP0:start ;start here").group("opLabel") == "start")
    assert(op0lineGroupReg.findAllIn(" 1: NOP0:start ;start here").group("comment") == "start here")
  }

  it must "define paramGroup correctly" in {
    val regEx: Regex = paramGroup.r
    assert(regEx.matches(" 123"))
    assert(regEx.matches(" @123"))
    assert(regEx.matches(",@123+B:here"))
    assert(regEx.matches(",@there:here"))
    assert(!regEx.matches("there:here"))
  }

  it must "define op1lineGroupReg correctly" in {
    val regEx: Regex = op1lineGroupReg
    assert(regEx.matches(" 1: NOP1 123"))
    assert(regEx.matches(" 1: IN 123"))
    assert(regEx.matches(" 1: OUT 123"))
    assert(regEx.matches(" 1: INCB 123"))
    assert(regEx.matches(" 1: INCB -123"))
    assert(!regEx.matches(" 1: IN"))
    val line1 = " 1: NOP1:start @123+B ;start here"
    assert(regEx.matches(line1))
    assert(regEx.findAllIn(line1).group("bytePos") == "1")
    assert(regEx.findAllIn(line1).group("op") == "NOP1")
    assert(regEx.findAllIn(line1).group("opLabel") == "start")
    assert(regEx.findAllIn(line1).group("param") == "123+B")
    assert(regEx.findAllIn(line1).group("positional") == "@")
    assert(regEx.findAllIn(line1).group("number") == "123")
    assert(regEx.findAllIn(line1).group("referencedLabel") == null)
    assert(regEx.findAllIn(line1).group("relative") == "+B")
    assert(regEx.findAllIn(line1).group("createdLabel") == null)
    assert(regEx.findAllIn(line1).group("comment") == "start here")
    val line2 = " 2: IN,@end+B:some ;end here"
    assert(regEx.matches(line2))
    assert(regEx.findAllIn(line2).group("bytePos") == "2")
    assert(regEx.findAllIn(line2).group("op") == "IN")
    assert(regEx.findAllIn(line2).group("opLabel") == null)
    assert(regEx.findAllIn(line2).group("param") == "end+B")
    assert(regEx.findAllIn(line2).group("positional") == "@")
    assert(regEx.findAllIn(line2).group("number") == null)
    assert(regEx.findAllIn(line2).group("referencedLabel") == "end")
    assert(regEx.findAllIn(line2).group("relative") == "+B")
    assert(regEx.findAllIn(line2).group("createdLabel") == "some")
    assert(regEx.findAllIn(line2).group("comment") == "end here")
  }

  it must "define op2lineGroupReg correctly" in {
    val regEx: Regex = op2lineGroupReg
    assert(regEx.matches(" 1: NOP2 123 @321"))
    assert(regEx.matches(" 1: JNEZ 123 @321"))
    assert(regEx.matches(" 1: JEQZ 123 @321"))
    assert(!regEx.matches(" 1: JNEZ 123"))
    val line1 = " 1: JNEZ:start @123+B @tgt:src ;start here"
    assert(regEx.matches(line1))
    assert(regEx.findAllIn(line1).group("bytePos") == "1")
    assert(regEx.findAllIn(line1).group("op") == "JNEZ")
    assert(regEx.findAllIn(line1).group("opLabel") == "start")
    assert(regEx.findAllIn(line1).group("param1") == "123+B")
    assert(regEx.findAllIn(line1).group("positional1") == "@")
    assert(regEx.findAllIn(line1).group("number1") == "123")
    assert(regEx.findAllIn(line1).group("referencedLabel1") == null)
    assert(regEx.findAllIn(line1).group("relative1") == "+B")
    assert(regEx.findAllIn(line1).group("createdLabel1") == null)
    assert(regEx.findAllIn(line1).group("param2") == "tgt")
    assert(regEx.findAllIn(line1).group("positional2") == "@")
    assert(regEx.findAllIn(line1).group("number2") == null)
    assert(regEx.findAllIn(line1).group("referencedLabel2") == "tgt")
    assert(regEx.findAllIn(line1).group("relative2") == null)
    assert(regEx.findAllIn(line1).group("createdLabel2") == "src")
    assert(regEx.findAllIn(line1).group("comment") == "start here")
  }

  it must "define op3lineGroupReg correctly" in {
    val regEx: Regex = op3lineGroupReg
    assert(regEx.matches(" 1: NOP3 123 @321 @tgt"))
    assert(regEx.matches(" 1: ADD 123 @321 @tgt"))
    assert(regEx.matches(" 1: MULT 123 @321 @tgt"))
    assert(regEx.matches(" 1: LESS 123 @321 @tgt"))
    assert(regEx.matches(" 1: EQ 123 @321 @tgt"))
    assert(!regEx.matches(" 1: ADD 123 @321"))
    val line1 = " 1: ADD:start @123+B 321 @tgt:src ;start here"
    assert(regEx.findAllIn(line1).group("bytePos") == "1")
    assert(regEx.findAllIn(line1).group("op") == "ADD")
    assert(regEx.findAllIn(line1).group("opLabel") == "start")
    assert(regEx.findAllIn(line1).group("param1") == "123+B")
    assert(regEx.findAllIn(line1).group("positional1") == "@")
    assert(regEx.findAllIn(line1).group("number1") == "123")
    assert(regEx.findAllIn(line1).group("referencedLabel1") == null)
    assert(regEx.findAllIn(line1).group("relative1") == "+B")
    assert(regEx.findAllIn(line1).group("createdLabel1") == null)
    assert(regEx.findAllIn(line1).group("param2") == "321")
    assert(regEx.findAllIn(line1).group("positional2") == null)
    assert(regEx.findAllIn(line1).group("number2") == "321")
    assert(regEx.findAllIn(line1).group("referencedLabel2") == null)
    assert(regEx.findAllIn(line1).group("relative2") == null)
    assert(regEx.findAllIn(line1).group("createdLabel2") == null)
    assert(regEx.findAllIn(line1).group("param3") == "tgt")
    assert(regEx.findAllIn(line1).group("positional3") == "@")
    assert(regEx.findAllIn(line1).group("number3") == null)
    assert(regEx.findAllIn(line1).group("referencedLabel3") == "tgt")
    assert(regEx.findAllIn(line1).group("relative3") == null)
    assert(regEx.findAllIn(line1).group("createdLabel3") == "src")
    assert(regEx.findAllIn(line1).group("comment") == "start here")
  }

  behavior of "opLookup"
  it should "handle all possible values" in {
    println(opLookup)
    assert(opLookup.contains("NOP0"))
    assert(opLookup("NOP0") == NOP0)
    assert(opLookup("NOP0").id == 0)
    assert(opLookup("NOP1") == NOP1)
    assert(opLookup("NOP1").id == 0)
    assert(opLookup("ADD") == ADD)
    assert(opLookup("ADD").id == 1)
    assert(opLookup("END") == END)
    assert(opLookup("END").id == 99)
  }

  behavior of "assemble"
  it should "handle single lines" in {
    assert(Assembler.assemble(Seq(" 0: NOP0")) == Seq(0))
    assert(Assembler.assemble(Seq(" 0: NOP1 @1")) == Seq(0, 1))
    assert(Assembler.assemble(Seq(" 0: NOP2 1 2")) == Seq(1100, 1, 2))
    assert(Assembler.assemble(Seq(" 0: NOP3 1 2 3")) == Seq(11100, 1, 2, 3))
    assert(Assembler.assemble(Seq(" 0: ADD 0 0 @0")) == Seq(1101, 0, 0, 0))
    assert(Assembler.assemble(Seq(" 0: MULT @1 @2 @3")) == Seq(2, 1, 2, 3))
    assert(Assembler.assemble(Seq(" 0: IN @1+B")) == Seq(203, 1))
    assert(Assembler.assemble(Seq(" 0: OUT 1")) == Seq(104, 1))
    assert(Assembler.assemble(Seq(" 0: JNEZ 1 2")) == Seq(1105, 1, 2))
    assert(Assembler.assemble(Seq(" 0: JEQZ @1 @2")) == Seq(6, 1, 2))
    assert(Assembler.assemble(Seq(" 0: LESS @1+B,@2+B,@3+B")) == Seq(22207, 1, 2, 3))
    assert(Assembler.assemble(Seq(" 0: EQ 1, 2, @3")) == Seq(1108, 1, 2, 3))
    assert(Assembler.assemble(Seq(" 0: INCB @1")) == Seq(9, 1))
    assert(Assembler.assemble(Seq(" 0: END")) == Seq(99))
  }

  it should "handle labels" in {
    assert(Assembler.assemble(Seq(" 0: NOP1:start @start")) == Seq(0, 0))
    assert(Assembler.assemble(Seq(" 0: ADD 1, 5, @2")) == Seq(1101, 1, 5, 2))
    assert(Assembler.assemble(Seq(" 0: ADD 1, 5:dest, @dest")) == Seq(1101, 1, 5, 2))
    assert(Assembler.assemble(Seq(" 0: ADD @dest, 5:dest, @10")) == Seq(1001, 2, 5, 10))
    assert(Assembler.assemble(Seq(" 0: JNEZ 5:dest, dest")) == Seq(1105, 5, 1))
  }

  it should "disallow +B without @" in {
    assertThrows[ParseException](Assembler.assemble(Seq(" 0: IN 1+B")))
  }

  it should "disallow immediates in write-only params" in {
    assertThrows[ParseException](Assembler.assemble(Seq(" 0: ADD 0 0 0")))
    assertThrows[ParseException](Assembler.assemble(Seq(" 0: MULT @1 @2 3")))
    assertThrows[ParseException](Assembler.assemble(Seq(" 0: IN 1")))
    assertThrows[ParseException](Assembler.assemble(Seq(" 0: LESS @1 @2 3")))
    assertThrows[ParseException](Assembler.assemble(Seq(" 0: EQ @1 @2 3")))
  }

  it should "handle multiple lines" in {
    val lines: Seq[String] = Seq(
      "; This example results in the following machine code",
      "; 109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,100,0,6,101,0,99",
      " 0: INCB:begin 1       ; this label is at position 0, so it is given then value 0",
      " 2: OUT @-1+B          ; this parameter evaluates to the value at position (-1 + the relative base)",
      " 4: ADD @100 1 @100",
      " 8: EQ @100 16 @101",
      "12: JEQZ @100, begin   ; this label revolves to an immediate parameter with the value 0",
      "15: JEQZ @101, @begin  ; this label resolves to a positional parameter with the value 0",
      "18: END",
    )
    assert(Assembler.assemble(lines) == Seq(109, 1, 204, -1, 1001, 100, 1, 100, 1008, 100, 16, 101, 1006, 100, 0, 6, 101, 0, 99))
  }
}
