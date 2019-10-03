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

  def printGrid(grid: Maze, size: Int): Unit = {

    print(" ")
    for (i <- 0 until size) {
      print("_ ")
    }
    println()
    for (i <- 0 until size) {
      for (j <- 0 until size) {
        val node = grid((i, j))
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

      }
      print("|")
      println()
    }
  }

}