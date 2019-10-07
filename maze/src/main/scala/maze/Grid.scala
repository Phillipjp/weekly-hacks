package maze

import maze.Domain._

object Grid {

  def makeGrid(size: Int): Maze = {
    val start = (0, 0)
    val gridSeq: Seq[Coord] = Stream.iterate(start)(cell => nextCell(cell, size - 1, 0)).takeWhile(_._1 < size)

    gridSeq
      .toList
      .map(coord => (coord, Node()))
      .toMap

  }

  private def nextCell(current: Coord, size: Int, xReset: Int): Coord = {
    if (current._2 < size)
      (current._1, current._2 + 1)
    else
      (current._1 + 1, xReset)
  }

  def printGrid(maze: Maze, size: Int): Unit = {

    val endCoords = maze.filter{case (_, node) => node.finshNode}.head._1
    printTopWall(size, endCoords)
    for (x <- 0 until size) {
      for (y <- 0 until size) {
        val node = maze((x, y))
        if (node.finshNode) {
          printFinishNode(size, x, y, node)
        }
        else {
          printNode(size, y, node)
        }
      }
      println()
    }
  }

  private def printTopWall(size: Int, endCoords: (Int, Int)) = {
    print(" ")
    if (endCoords._1 == 0) {
      for (i <- 0 until size) {
        if (i == endCoords._2) print("  ")
        else print("_ ")
      }
    }
    else {
      for (i <- 0 until size) {
        print("_ ")
      }
    }
    println()
  }

  private def printNode(size: Int, y: Int, node: Node) = {
    val walls = (node.leftWall, node.bottomWall)
    walls match {
      case (true, true) =>
        print("|_")
      case (true, false) =>
        print("| ")
      case (false, true) =>
        print(" _")
      case (false, false) =>
        print("  ")
    }
    if (y == size - 1) print("|")
  }

  private def printFinishNode(size: Int, i: Int, j: Int, node: Node) = {
    (i, j) match {
      case s if s._1 == 0 =>
        if (node.bottomWall && node.leftWall) print("|_")
        else if (node.bottomWall) print(" _")
        else if (node.leftWall) print("| ")
        else print("  ")
      case s if s._2 == 0 =>
        if (node.bottomWall) print(" _")
        else print("  ")
      case s if s._1 == size - 1 =>
        if (node.leftWall) print("| ")
        else print("  ")
      case s if s._2 == size - 1 =>
        if (node.bottomWall && node.leftWall) print("|_")
        else if (node.bottomWall) print(" _")
        else if (node.leftWall) print("| ")
        else print("  ")

    }
  }
}