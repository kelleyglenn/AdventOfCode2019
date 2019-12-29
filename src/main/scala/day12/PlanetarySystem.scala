package day12

class PlanetarySystem() {
  def stepForward(moons: Set[Moon]): Set[Moon] = {
    moons
      .map(
        (m: Moon) => {
          val newX: Int = m.velocity.x + moons.count(
            _.position.x > m.position.x
          ) - moons.count(_.position.x < m.position.x)
          val newY: Int = m.velocity.y + moons.count(
            _.position.y > m.position.y
          ) - moons.count(_.position.y < m.position.y)
          val newZ: Int = m.velocity.z + moons.count(
            _.position.z > m.position.z
          ) - moons.count(_.position.z < m.position.z)
          m.copy(velocity = Cartesian(newX.toShort, newY.toShort, newZ.toShort))
        }
      )
      .map((m: Moon) => {
        m.copy(
          position = Cartesian(
            (m.position.x + m.velocity.x).toShort,
            (m.position.y + m.velocity.y).toShort,
            (m.position.z + m.velocity.z).toShort
          )
        )
      })
  }

  def stepBackward(moons: Set[Moon]): Set[Moon] = {
    val oldMoons: Set[Moon] =
      moons.map((m: Moon) => {
        m.copy(
          position = Cartesian(
            (m.position.x - m.velocity.x).toShort,
            (m.position.y - m.velocity.y).toShort,
            (m.position.z - m.velocity.z).toShort
          )
        )
      })
    oldMoons.map(
      (m: Moon) => {
        val newX: Int = m.velocity.x + oldMoons.count(
          _.position.x < m.position.x
        ) - oldMoons.count(_.position.x > m.position.x)
        val newY: Int = m.velocity.y + oldMoons.count(
          _.position.y < m.position.y
        ) - oldMoons.count(_.position.y > m.position.y)
        val newZ: Int = m.velocity.z + oldMoons.count(
          _.position.z < m.position.z
        ) - oldMoons.count(_.position.z > m.position.z)
        m.copy(velocity = Cartesian(newX.toShort, newY.toShort, newZ.toShort))
      }
    )
  }

  def stepCountUntilStateRepeated(moons: Set[Moon]): Long = {
    var curState: Set[Moon] = moons
    var answer: Long = -1
    var i: Long = 0
//    var potential = curState.map(_.energy).sum
//    var kinetic = curState.map(_.energy).sum
//    println("step(" + i + ") total potential, kinetic, sum, product = " +
//      potential + " , " + kinetic + " , " + (potential + kinetic) + " , " + potential * kinetic)
    while (answer == -1 && i <= Long.MaxValue) {
      curState = stepForward(curState)
      i += 1
//      potential = curState.map(_.energy).sum
//      kinetic = curState.map(_.energy).sum
//      println("step(" + i + ") total potential, kinetic, sum, product = " +
//        potential + " , " + kinetic + " , " + (potential + kinetic) + " , " + potential * kinetic)
//      if (i % 1000000 == 0) println(i)
      if (curState == moons) {
        answer = i
      }
    }
    answer
  }
}

case class Cartesian(x: Short, y: Short, z: Short) {
  def energy: Int = Math.abs(x) + Math.abs(y) + Math.abs(z)

  def toLong: Long = {
    x * 150 * 150 + y * 150 + z
  }
}

case class Moon(position: Cartesian, velocity: Cartesian) {
  def potentialEnergy: Int = position.energy

  def kineticEnergy: Int = velocity.energy

  def totalEnergy: Int = potentialEnergy * kineticEnergy

  def toLong: Long = {
    position.toLong * 3000000 + velocity.toLong
  }
}
