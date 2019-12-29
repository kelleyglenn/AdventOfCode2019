package day15

import java.util.concurrent.BlockingQueue

class RepairDroid(val computerInput: BlockingQueue[Long],
                  val computerOutput: BlockingQueue[Long]) {

  object Direction extends Enumeration {
    val North: Value = Value(1)
    val South: Value = Value(2)
    val West: Value = Value(3)
    val East: Value = Value(4)
  }

  object Status extends Enumeration {
    val Wall, Space, OxygenSystem = Value
  }

  case class Position(x: Int, y: Int) // 0,0 is upper-left

  case class Spot(pos: Position)

  trait Wall

  trait OxygenSystem

  class Space(override val pos: Position) extends Spot(pos) {
    var north: Option[Spot] = None
    var south: Option[Spot] = None
    var west: Option[Spot] = None
    var east: Option[Spot] = None
  }

  val graphHead: Space = new Space(Position(0, 0))
  var curSpace: Space = graphHead
  var oxygenSystem: Option[Spot with OxygenSystem] = None
  var spacesToExplore: Set[Space] = Set(graphHead)
  var directionFacing: Direction.Value = Direction.North
  var allSpaces: Set[Space] = Set(graphHead)
  var allWalls: Set[Spot with Wall] = Set.empty

  //  def nearestEmptyToExplore: Space = {
  //
  //  }
  //
  //  def directionsToTarget: Seq[Direction.Value] = {
  //
  //  }

  def createGraph(): Unit = {
    createEdges()
  }

  def spotAsSpace(spot: Spot): Space = {
    spot match {
      case s: Space => s
      case _ =>
        throw new IllegalStateException("Somehow moved forward into a wall")
    }
  }

  def newSpace(pos: Position): Space = {
    val existingSpace: Option[Space] =
      allSpaces.find((s: Space) => s.pos == pos)
    if (existingSpace.isEmpty) {
      val newSpot = new Space(pos)
      allSpaces += newSpot
      newSpot
    } else existingSpace.get
  }

  def newWall(pos: Position): Spot = {
    val newWall = new Spot(pos) with Wall
    allWalls += newWall
    newWall
  }

  def newOxygenSystem(pos: Position): Spot with OxygenSystem = {
    new Spot(pos) with OxygenSystem
  }

  def allSpots: Set[Spot] = allSpaces ++ allWalls

  def moveForward(): Space = {
    directionFacing match {
      case Direction.North =>
        val nextSpace: Space = {
          if (curSpace.north.isEmpty) {
            val newOne: Space = newSpace(
              Position(curSpace.pos.x, curSpace.pos.y - 1)
            )
            curSpace.north = Some(newOne)
            newOne
          } else spotAsSpace(curSpace.north.get)
        }
        if (nextSpace.south.isEmpty)
          nextSpace.south = Some(curSpace)
        curSpace = nextSpace
      case Direction.South =>
        val nextSpace: Space = {
          if (curSpace.south.isEmpty) {
            val newOne: Space = newSpace(
              Position(curSpace.pos.x, curSpace.pos.y + 1)
            )
            curSpace.south = Some(newOne)
            newOne
          } else spotAsSpace(curSpace.south.get)
        }
        if (nextSpace.north.isEmpty)
          nextSpace.north = Some(curSpace)
        curSpace = nextSpace
      case Direction.West =>
        val nextSpace: Space = {
          if (curSpace.west.isEmpty) {
            val newOne: Space = newSpace(
              Position(curSpace.pos.x - 1, curSpace.pos.y)
            )
            curSpace.west = Some(newOne)
            newOne
          } else spotAsSpace(curSpace.west.get)
        }
        if (nextSpace.east.isEmpty)
          nextSpace.east = Some(curSpace)
        curSpace = nextSpace
      case Direction.East =>
        val nextSpace: Space = {
          if (curSpace.east.isEmpty) {
            val newOne: Space = newSpace(
              Position(curSpace.pos.x + 1, curSpace.pos.y)
            )
            curSpace.east = Some(newOne)
            newOne
          } else spotAsSpace(curSpace.east.get)
        }
        if (nextSpace.west.isEmpty)
          nextSpace.west = Some(curSpace)
        curSpace = nextSpace
    }
    curSpace
  }

  def tryToMoveForward: Status.Value = {
    computerInput.put(directionFacing.id.toLong)
    Status(computerOutput.take().toInt)
  }

  def turnRight: Direction.Value = {
    directionFacing match {
      case Direction.North => Direction.East
      case Direction.South => Direction.West
      case Direction.West  => Direction.North
      case Direction.East  => Direction.South
    }
  }

  def turnLeft: Direction.Value = {
    directionFacing match {
      case Direction.North => Direction.West
      case Direction.South => Direction.East
      case Direction.West  => Direction.South
      case Direction.East  => Direction.North
    }
  }

  def turnAround: Direction.Value = {
    directionFacing match {
      case Direction.North => Direction.South
      case Direction.South => Direction.North
      case Direction.West  => Direction.East
      case Direction.East  => Direction.West
    }
  }

  // If (there's not a wall on your left) turn left and move forward
  // Else if (there's a not a wall in front of you) move forward
  // Else if (there's not a wall on your right) turn right and move forward
  // Else turn around and move forward
  def moveWithWallOnLeft(): Space = {
    directionFacing = turnLeft
    var latestStatus: Status.Value = tryToMoveForward
    if (latestStatus == Status.Wall) {
      wallInFront()
      directionFacing = turnRight
      latestStatus = tryToMoveForward
      if (latestStatus == Status.Wall) {
        wallInFront()
        directionFacing = turnRight
        latestStatus = tryToMoveForward
        if (latestStatus == Status.Wall) {
          wallInFront()
          directionFacing = turnRight
          latestStatus = tryToMoveForward
          if (latestStatus == Status.Wall) {
            wallInFront()
            throw new IllegalStateException("I'm trapped in a box!")
          }
        }
      }
    }
    curSpace = moveForward()
    if (latestStatus == Status.OxygenSystem)
      oxygenSystem = Some(newOxygenSystem(curSpace.pos))
    curSpace
  }

  def wallInFront(): Unit = {
    directionFacing match {
      case Direction.North =>
        if (curSpace.north.isEmpty)
          curSpace.north = Some(
            newWall(Position(curSpace.pos.x, curSpace.pos.y - 1))
          )
      case Direction.South =>
        if (curSpace.south.isEmpty)
          curSpace.south = Some(
            newWall(Position(curSpace.pos.x, curSpace.pos.y + 1))
          )
      case Direction.West =>
        if (curSpace.west.isEmpty)
          curSpace.west = Some(
            newWall(Position(curSpace.pos.x - 1, curSpace.pos.y))
          )
      case Direction.East =>
        if (curSpace.east.isEmpty)
          curSpace.east = Some(
            newWall(Position(curSpace.pos.x + 1, curSpace.pos.y))
          )
    }
  }

  def createEdges(): Unit = {
    // Go forward until you hit a wall. Consider this the beginning. Turn right.
    // Follow the wall on your left until you get back to the beginning
    var latestStatus: Status.Value = tryToMoveForward
    while (latestStatus != Status.Wall) {
      moveForward()
      if (latestStatus == Status.OxygenSystem)
        oxygenSystem = Some(newOxygenSystem(curSpace.pos))
      latestStatus = tryToMoveForward
    }
    wallInFront()
    val beginning: Space = curSpace
    directionFacing = turnRight
    curSpace = moveWithWallOnLeft()
    while (curSpace != beginning) curSpace = moveWithWallOnLeft()
  }
}
