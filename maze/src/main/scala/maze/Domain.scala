package maze

object Domain {

  case class Node(visited: Boolean = false, leftWall:Boolean = true, bottomWall:Boolean = true, start: Boolean = false, end: Boolean = false)

  type Coord = (Int, Int)
  type Maze = Map[Coord, Node]
}
