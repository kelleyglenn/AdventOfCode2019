package day14

import org.scalatest.flatspec.AnyFlatSpec

import scala.io.{BufferedSource, Source}
import scala.util.matching.Regex

class StoichiometryTest extends AnyFlatSpec {
  behavior of "reactantsNeeded"
  it should "handle simple case" in {
    var s = new Stoichiometry(
      Set(Reaction(Set(Reactant("ORE", 1)), Reactant("FUEL", 1)))
    )
    assert(s.reactantsNeeded(Reactant("FUEL", 1)) == Set(Reactant("ORE", 1)))
    assert(s.stockpile == Map.empty)
    s = new Stoichiometry(
      Set(Reaction(Set(Reactant("ORE", 1)), Reactant("FUEL", 10)))
    )
    assert(s.reactantsNeeded(Reactant("FUEL", 1)) == Set(Reactant("ORE", 1)))
    assert(s.stockpile == Map(("FUEL", Reactant("FUEL", 9))))
  }

  behavior of "calcElementRequiredForReactant"
  it should "handle simple case" in {
    var s = new Stoichiometry(
      Set(Reaction(Set(Reactant("ORE", 1)), Reactant("FUEL", 1)))
    )
    assert(s.calcElementRequiredForReactant("ORE", Reactant("FUEL", 1)) == 1)
    s = new Stoichiometry(
      Set(Reaction(Set(Reactant("ORE", 1)), Reactant("FUEL", 10)))
    )
    assert(s.calcElementRequiredForReactant("ORE", Reactant("FUEL", 10)) == 1)
    assert(s.calcElementRequiredForReactant("ORE", Reactant("FUEL", 11)) == 2)
  }

  behavior of "calcElementRequiredForReactants"
  it should "handle simple case" in {
    var s = new Stoichiometry(
      Set(Reaction(Set(Reactant("ORE", 1)), Reactant("FUEL", 1)))
    )
    assert(
      s.calcElementRequiredForReactants("ORE", Set(Reactant("FUEL", 1))) == 1
    )
    s = new Stoichiometry(
      Set(Reaction(Set(Reactant("ORE", 1)), Reactant("FUEL", 10)))
    )
    assert(
      s.calcElementRequiredForReactants("ORE", Set(Reactant("FUEL", 10))) == 1
    )
    assert(
      s.calcElementRequiredForReactants(
        "ORE",
        Set(Reactant("FUEL", 11), Reactant("FUEL", 9))
      ) == 2
    )
  }

  behavior of "calcElementCountRequiredForTarget"
  it should "handle simple case" in {
    var s = new Stoichiometry(
      Set(Reaction(Set(Reactant("ORE", 1)), Reactant("FUEL", 1)))
    )
    assert(s.calcElementCountRequiredForTarget("ORE", "FUEL") == 1)
    s = new Stoichiometry(
      Set(Reaction(Set(Reactant("ORE", 1)), Reactant("FUEL", 10)))
    )
    assert(s.calcElementCountRequiredForTarget("ORE", "FUEL") == 1)
    s = new Stoichiometry(
      Set(Reaction(Set(Reactant("ORE", 10)), Reactant("FUEL", 1)))
    )
    assert(s.calcElementCountRequiredForTarget("ORE", "FUEL") == 10)
  }

  it should "handle two step case" in {
    val s = new Stoichiometry(
      Set(
        Reaction(Set(Reactant("ORE", 3)), Reactant("A", 2)),
        Reaction(Set(Reactant("A", 10)), Reactant("FUEL", 1))
      )
    )
    assert(s.calcElementCountRequiredForTarget("ORE", "FUEL") == 15)
  }

  it should "handle multiple computerInput case" in {
    val s = new Stoichiometry(
      Set(
        Reaction(Set(Reactant("ORE", 3)), Reactant("A", 2)),
        Reaction(Set(Reactant("ORE", 4)), Reactant("B", 2)),
        Reaction(Set(Reactant("A", 10), Reactant("B", 4)), Reactant("FUEL", 1))
      )
    )
    assert(s.calcElementCountRequiredForTarget("ORE", "FUEL") == 23)
  }

  it should "handle puzzle example 1" in new SetupPuzzleData("example1") {
    val s = new Stoichiometry(reactions)
    assert(s.calcElementCountRequiredForTarget("ORE", "FUEL") == 31)
  }

  it should "handle puzzle example 2" in new SetupPuzzleData("example2") {
    val s = new Stoichiometry(reactions)
    assert(s.calcElementCountRequiredForTarget("ORE", "FUEL") == 165)
  }

  it should "handle puzzle example 3" in new SetupPuzzleData("example3") {
    val s = new Stoichiometry(reactions)
    assert(s.calcElementCountRequiredForTarget("ORE", "FUEL") == 13312)
  }

  it should "handle puzzle example 4" in new SetupPuzzleData("example4") {
    val s = new Stoichiometry(reactions)
    assert(s.calcElementCountRequiredForTarget("ORE", "FUEL") == 180697)
  }

  it should "handle puzzle example 5" in new SetupPuzzleData("example5") {
    var s = new Stoichiometry(reactions)
    assert(s.calcElementCountRequiredForTarget("ORE", "CNZTR") == 171)
    assert(s.stockpile == Map(("CNZTR", Reactant("CNZTR", 7))))
    s = new Stoichiometry(reactions)
    assert(
      s.calcElementCountRequiredForTarget("ORE", "XDBXC") == (171 * 4) + (121 * 2)
    )
    assert(
      s.stockpile == Map(
        ("XDBXC", Reactant("XDBXC", 1)),
        ("CNZTR", Reactant("CNZTR", 5)),
        ("VRPVC", Reactant("VRPVC", 2))
      )
    )
//    assert(s.calcElementCountRequiredForTarget("ORE", "FUEL") == 2210736)
  }

  it should "solve the first puzzle" in new SetupPuzzleData("input") {
    val s = new Stoichiometry(reactions)
    assert(s.calcElementCountRequiredForTarget("ORE", "FUEL") == 337075)
  }

  //  it should "solve the second puzzle" in new SetupPuzzleData("computerInput") {
  //    val s = new Stoichiometry(reactions)
  //    var fuelProduced:Long = 0
  //    var oreNeeded:Long = s.calcElementCountRequiredForTarget("ORE", "FUEL")
  //    while (oreNeeded <= 1000000000000L){
  //      fuelProduced += 1
  //      if (fuelProduced % 10000 == 0) println(oreNeeded)
  //      if (s.stockpile == Map.empty) println(""+oreNeeded+" ore produces " + fuelProduced + " fuel")
  //      oreNeeded += s.calcElementCountRequiredForTarget("ORE", "FUEL")
  //    }
  //    assert(fuelProduced == 12)
  //  }

  class SetupPuzzleData(name: String) {
    val bufferedSource: BufferedSource =
      Source.fromURL(getClass.getResource("/day14/" + name + ".txt"))
    val reactions: Set[Reaction] = bufferedSource.getLines.toSeq
      .map((l: String) => {
        val firstLevel: Regex =
          raw"(.*) => (\d*) (\w*)".r("reactants", "productCt", "productName")
        val groups: Regex.MatchIterator = firstLevel.findAllIn(l)
        val reactantStrings: Array[String] =
          groups.group("reactants").split(',')
        Reaction(
          reactantStrings
            .map(
              (rs: String) =>
                Reactant(rs.trim.split(' ')(1), rs.trim.split(' ')(0).toInt)
            )
            .toSet,
          Reactant(groups.group("productName"), groups.group("productCt").toInt)
        )
      })
      .toSet
    bufferedSource.close()
  }
}
