package maze

object Domain {

  case class Node(visited: Boolean = false, leftWall:Boolean = true, bottomWall:Boolean = true, edgeQueueNumber: Int = -1, lastEdge: Boolean = false)

  type Coord = (Int, Int)
  type Maze = Map[Coord, Node]
}
