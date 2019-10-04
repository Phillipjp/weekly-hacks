package maze

import maze.Domain.{Coord, Maze, Node}

import scala.util.Random

object DepthFirstSearch {

  def generateMaze(maze: Maze, size: Int): Maze = {
    val startCoords = (0,0)

    def depthFirstSearch(nodeCoords: Coord, queue: Seq[Coord], maze: Maze, size: Int): Maze = {
//      Grid.printGrid(maze, size+1)
      val newQueue = queue :+ nodeCoords
      val node = maze(nodeCoords)
      val visitedMaze = maze + (nodeCoords -> Node(visited = true, leftWall = node.leftWall, bottomWall = node.bottomWall, edgeQueueNumber = node.edgeQueueNumber))
      if(visitedMaze.values.forall(node => node.visited)) {
        visitedMaze
      }
      else if(getAdjacentNodes(nodeCoords, visitedMaze).forall(coord => visitedMaze(coord).visited)){
        val visitedNodes = newQueue.reverse.takeWhile(coord => getAdjacentNodes(coord, visitedMaze).forall(c => visitedMaze(c).visited))
        val updatedQueue = newQueue.dropRight(visitedNodes.size)
        val currentNode = updatedQueue.last
        val nextNodeCoords = randomAdjacentNode(currentNode, visitedMaze)
        val updatedMaze: Maze = updateMaze(currentNode, size, updatedQueue, visitedMaze, nextNodeCoords)
        depthFirstSearch(nextNodeCoords, updatedQueue, updatedMaze, size)
      }
      else{
        val nextNodeCoords = randomAdjacentNode(nodeCoords, visitedMaze)
        val updatedMaze: Maze = updateMaze(nodeCoords, size, newQueue, visitedMaze, nextNodeCoords)
        depthFirstSearch(nextNodeCoords, newQueue, updatedMaze, size)
      }

    }
    val finishedMaze = depthFirstSearch(startCoords, Seq(), maze, size)
    val startNode = finishedMaze(startCoords)
    val startMaze = finishedMaze + (startCoords -> Node(startNode.visited, leftWall = false, startNode.bottomWall))
    val (endNodeCoords, endNode) = startMaze.maxBy{case (coords, node) => node.edgeQueueNumber}
    startMaze + (endNodeCoords -> Node(visited = endNode.visited, leftWall = endNode.leftWall, bottomWall = endNode.bottomWall, edgeQueueNumber = endNode.edgeQueueNumber, lastEdge = true))
  }

  private def updateMaze(nodeCoords: (Int, Int), size: Int, newQueue: Seq[(Int, Int)], visitedMaze: Map[(Int, Int), Node], nextNodeCoords: (Int, Int)) = {
    val updatedMaze = if (isEdge(nextNodeCoords, size)) {
      val updatedWallsMaze = updateMazeWalls(nodeCoords, visitedMaze, nextNodeCoords)
      val nextNode = updatedWallsMaze(nextNodeCoords)
      updatedWallsMaze + (nextNodeCoords -> Node(nextNode.visited, nextNode.leftWall, nextNode.bottomWall, newQueue.size))
    }
    else {
      updateMazeWalls(nodeCoords, visitedMaze, nextNodeCoords)
    }
    updatedMaze
  }

  private def updateMazeWalls(nodeCoords: (Int, Int), visitedMaze: Map[(Int, Int), Node], nextNodeCoords: (Int, Int)): Maze = {
    nextNodeCoords match {
      case c if c._2 == nodeCoords._2 + 1 => visitedMaze + (nextNodeCoords -> Node(visited = visitedMaze(nextNodeCoords).visited, leftWall = false, bottomWall = visitedMaze(nextNodeCoords).bottomWall, edgeQueueNumber = visitedMaze(nextNodeCoords).edgeQueueNumber))
      case c if c._1 == nodeCoords._1 + 1 => visitedMaze + (nodeCoords -> Node(visited = visitedMaze(nodeCoords).visited, leftWall = visitedMaze(nodeCoords).leftWall, bottomWall = false, edgeQueueNumber = visitedMaze(nodeCoords).edgeQueueNumber))
      case c if c._2 == nodeCoords._2 - 1 => visitedMaze + (nodeCoords -> Node(visited = visitedMaze(nodeCoords).visited, leftWall = false, bottomWall = visitedMaze(nodeCoords).bottomWall, edgeQueueNumber = visitedMaze(nodeCoords).edgeQueueNumber))
      case c if c._1 == nodeCoords._1 - 1 => visitedMaze + (nextNodeCoords -> Node(visited = visitedMaze(nextNodeCoords).visited, leftWall = visitedMaze(nextNodeCoords).leftWall, bottomWall = false, edgeQueueNumber = visitedMaze(nextNodeCoords).edgeQueueNumber))
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

  private def isEdge(coord: Coord, size: Int): Boolean = {
    val coordSeq = Seq(coord._1, coord._2)
    coordSeq.contains(0) || coordSeq.contains(size)
  }
}
