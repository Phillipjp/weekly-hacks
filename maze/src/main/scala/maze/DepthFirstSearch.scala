package maze

import maze.Domain.{Coord, Maze, Node}

import scala.util.Random

object DepthFirstSearch {

  def generateMaze(maze: Maze): Maze = {
    val startCoords = (0,0)

    def depthFirstSearch(nodeCoords: Coord, queue: Seq[Coord], maze: Maze): Maze = {
      Grid.printGrid(maze, 5)
      val newQueue = queue :+ nodeCoords
      val node = maze(nodeCoords)
      val visitedMaze = maze + (nodeCoords -> Node(visited = true, leftWall = node.leftWall, bottomWall = node.bottomWall))
      if(visitedMaze.values.forall(node => node.visited)) {
        val endNode = maze(nodeCoords)
        maze + (nodeCoords -> Node(endNode.visited, endNode.leftWall, endNode.bottomWall, endNode.start, end = true))
      }
      else if(getAdjacentNodes(nodeCoords, visitedMaze).forall(coord => visitedMaze(coord).visited)){
        val visitedNodes = newQueue.reverse.takeWhile(coord => getAdjacentNodes(coord, visitedMaze).forall(c => visitedMaze(c).visited))
        val updatedQueue = newQueue.dropRight(visitedNodes.size)
        val currentNode = updatedQueue.last
        val nextNodeCoords = randomAdjacentNode(currentNode, visitedMaze)
        val updatedMaze: Map[(Int, Int), Node] = updateMaze(currentNode, visitedMaze, nextNodeCoords)
        depthFirstSearch(nextNodeCoords, updatedQueue, updatedMaze)
      }
      else{
        val nextNodeCoords = randomAdjacentNode(nodeCoords, visitedMaze)
        val updatedMaze = updateMaze(nodeCoords, visitedMaze, nextNodeCoords)
        depthFirstSearch(nextNodeCoords, newQueue, updatedMaze)
      }

    }
    val completeMaze = depthFirstSearch(startCoords, Seq(), maze)
    val startNode = completeMaze(startCoords)
    completeMaze + (startCoords -> Node(startNode.visited, leftWall = false, startNode.bottomWall, start =  true, startNode.end))
  }

  private def updateMaze(nodeCoords: (Int, Int), visitedMaze: Map[(Int, Int), Node], nextNodeCoords: (Int, Int)) = {
    nextNodeCoords match {
      case c if c._2 == nodeCoords._2 + 1 => visitedMaze + (nextNodeCoords -> Node(visited = visitedMaze(nextNodeCoords).visited, leftWall = false, bottomWall = visitedMaze(nextNodeCoords).bottomWall))
      case c if c._1 == nodeCoords._1 + 1 => visitedMaze + (nodeCoords -> Node(visited = visitedMaze(nodeCoords).visited, leftWall = visitedMaze(nodeCoords).leftWall, bottomWall = false))
      case c if c._2 == nodeCoords._2 - 1 => visitedMaze + (nodeCoords -> Node(visited = visitedMaze(nodeCoords).visited, leftWall = false, bottomWall = visitedMaze(nodeCoords).bottomWall))
      case c if c._1 == nodeCoords._1 - 1 => visitedMaze + (nextNodeCoords -> Node(visited = visitedMaze(nextNodeCoords).visited, leftWall = visitedMaze(nextNodeCoords).leftWall, bottomWall = false))
    }
  }

  private [maze] def randomAdjacentNode(nodeCoords: Coord, maze: Maze): Coord = {
    val adjacentNodes = getAdjacentNodes(nodeCoords, maze).filter(coord => !maze(coord).visited)
    val rand = Random
    adjacentNodes(rand.nextInt(adjacentNodes.length))
  }

  private def getAdjacentNodes(nodeCoords: (Int, Int), maze: Maze): Seq[(Int, Int)] = {
    val x = nodeCoords._1
    val y = nodeCoords._2
    Seq((x, y + 1), (x + 1, y), (x, y - 1), (x - 1, y)).filter(coord => maze.get(coord).isDefined)
  }
}
