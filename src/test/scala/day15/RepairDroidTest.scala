package day15

import java.util.concurrent.{BlockingQueue, LinkedBlockingQueue}

import org.scalatest.flatspec.AnyFlatSpec

class RepairDroidTest extends AnyFlatSpec {

  behavior of "createEdges"
  it should "handle a 3x3 space" in {
    val computerInput: BlockingQueue[Long] = new LinkedBlockingQueue[Long]()
    val computerOutput: BlockingQueue[Long] = new LinkedBlockingQueue[Long]()
    val droid: RepairDroid = new RepairDroid(computerInput, computerOutput)
    val Wall: droid.Status.Value = droid.Status.Wall
    val Space: droid.Status.Value = droid.Status.Space
    val P: droid.Position.type = droid.Position
    val outputs: Seq[droid.Status.Value] =
      Seq(
        Space,
        Wall, //straight
        Wall,
        Space,
        Wall,
        Wall, //to first corner
        Space,
        Wall,
        Space,
        Wall,
        Wall, //to second corner
        Space,
        Wall,
        Space,
        Wall,
        Wall, // to third corner
        Space,
        Wall,
        Space,
        Wall,
        Wall, // to fourth corner
        Space
      ) // to beginning
    outputs.foreach((v: droid.Status.Value) => computerOutput.put(v.id.toLong))
    droid.createGraph()
    assert(droid.allSpots.size == (3 * 3) + (3 * 4))
    assert(droid.allSpots.collect({ case w: droid.Wall => w }).size == (3 * 4))
    val allSpaces: Set[droid.Space] = droid.allSpots.collect({
      case s: droid.Space => s
    })
    assert(allSpaces.size == (3 * 3))
    assert(
      allSpaces
        .find((s: droid.Space) => s.pos.x == 0 && s.pos.y == -1)
        .get
        .east
        .get == new droid.Space(P(1, -1))
    )
    assert(
      allSpaces
        .find((s: droid.Space) => s.pos.x == 1 && s.pos.y == -1)
        .get
        .west
        .get == new droid.Space(P(0, -1))
    )
    assert(
      allSpaces
        .find((s: droid.Space) => s.pos.x == 0 && s.pos.y == -1)
        .get
        .west
        .get == new droid.Space(P(-1, -1))
    )
  }
}
