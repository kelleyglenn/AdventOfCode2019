package day12

import org.scalatest.flatspec.AnyFlatSpec

class PlanetarySystemTest extends AnyFlatSpec {

  class SetupExampleData {
    val m1: Moon = Moon(Cartesian(x = -1, y = 0, z = 2), Cartesian(0, 0, 0))
    val m2: Moon = Moon(Cartesian(x = 2, y = -10, z = -7), Cartesian(0, 0, 0))
    val m3: Moon = Moon(Cartesian(x = 4, y = -8, z = 8), Cartesian(0, 0, 0))
    val m4: Moon = Moon(Cartesian(x = 3, y = 5, z = -1), Cartesian(0, 0, 0))
    var moons = Set(m1, m2, m3, m4)
    val ps = new PlanetarySystem()

  }

  behavior of "stepForward"
  it should "handle examples" in new SetupExampleData {
    // After 1 stepForward
    moons = ps.stepForward(moons)
    assert(
      moons.contains(
        Moon(Cartesian(x = 2, y = -1, z = 1), Cartesian(x = 3, y = -1, z = -1))
      )
    )
    assert(
      moons.contains(
        Moon(Cartesian(x = 3, y = -7, z = -4), Cartesian(x = 1, y = 3, z = 3))
      )
    )
    assert(
      moons.contains(
        Moon(Cartesian(x = 1, y = -7, z = 5), Cartesian(x = -3, y = 1, z = -3))
      )
    )
    assert(
      moons.contains(
        Moon(Cartesian(x = 2, y = 2, z = 0), Cartesian(x = -1, y = -3, z = 1))
      )
    )
    //After 10 steps
    moons = ps.stepForward(moons)
    moons = ps.stepForward(moons)
    moons = ps.stepForward(moons)
    moons = ps.stepForward(moons)
    moons = ps.stepForward(moons)
    moons = ps.stepForward(moons)
    moons = ps.stepForward(moons)
    moons = ps.stepForward(moons)
    moons = ps.stepForward(moons)
    assert(
      moons.contains(
        Moon(Cartesian(x = 2, y = 1, z = -3), Cartesian(x = -3, y = -2, z = 1))
      )
    )
    assert(moons.map(_.totalEnergy).sum == 179)
  }

  class SetupPuzzleData {
    val m1: Moon = Moon(Cartesian(x = 6, y = 10, z = 10), Cartesian(0, 0, 0))
    val m2: Moon = Moon(Cartesian(x = -9, y = 3, z = 17), Cartesian(0, 0, 0))
    val m3: Moon = Moon(Cartesian(x = 9, y = -4, z = 14), Cartesian(0, 0, 0))
    val m4: Moon = Moon(Cartesian(x = 4, y = 14, z = 4), Cartesian(0, 0, 0))
    var moons = Set(m1, m2, m3, m4)
    val ps = new PlanetarySystem()
  }

  it should "solve the puzzle" in new SetupPuzzleData {
    (1 to 1000).foreach((_: Int) => moons = ps.stepForward(moons))
    assert(moons.map(_.totalEnergy).sum == 13045)
    println("Max Px=" + moons.map((m: Moon) => Math.abs(m.position.x)).max)
    println("Max Py=" + moons.map((m: Moon) => Math.abs(m.position.y)).max)
    println("Max Pz=" + moons.map((m: Moon) => Math.abs(m.position.z)).max)
    println("Max Vx=" + moons.map((m: Moon) => Math.abs(m.velocity.x)).max)
    println("Max Vy=" + moons.map((m: Moon) => Math.abs(m.velocity.y)).max)
    println("Max Vz=" + moons.map((m: Moon) => Math.abs(m.velocity.z)).max)
  }

  behavior of "stepBackward"
  it should "undo any stepForward" in new SetupExampleData {
    assert(ps.stepBackward(ps.stepForward(moons)) == moons)
  }

  behavior of "stepCountUntilStateRepeated"
  it should "handle the example" in new SetupExampleData {
    assert(ps.stepCountUntilStateRepeated(moons) == 2772)
  }

//  it should "let me stepForward the puzzle 4,294,967,295 times" in new SetupPuzzleData{
//    val start: Long = System.currentTimeMillis()
//    (Int.MinValue to Int.MaxValue).foreach(_ => moons = ps.stepForward(moons))
//    val end: Long = System.currentTimeMillis()
//    println("It took " + (end - start) + " milliseconds")
//    println("That is " + ((end - start)/(Int.MaxValue-Int.MinValue)) + " per second")
//  }

//  it should "solve the puzzle" in new SetupPuzzleData {
//    assert(ps.stepCountUntilStateRepeated(moons) == 1)
//  }
}
