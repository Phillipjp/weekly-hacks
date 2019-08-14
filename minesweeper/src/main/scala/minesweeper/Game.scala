package minesweeper

import minesweeper.domain.{Coord, Grid}

import scala.annotation.tailrec

class Game(grid: Grid, size: Int, numBombs: Int) {

  def run(): Unit = {
    val finalGrid = playGame(grid)
    Grid.printGrid(finalGrid, size)
  }


  @tailrec
  final def playGame(grid: Grid): Grid = {
    Grid.printGrid(grid, size)
    val revealedCells = getRevealedCells(grid)
    val input = getInput(revealedCells, grid.keySet.toList)
    val updatedGrid = Grid.revealCells(List(input), grid)
    if(checkForBomb(input, grid))
      updatedGrid
    else {
      if(getRevealedCells(updatedGrid).length == grid.keySet.toList.length - numBombs)
        updatedGrid
      else
        playGame(updatedGrid)
    }
  }

  @tailrec
  private def getInput(revealedCells: List[Coord], allCoords: List[Coord]): Coord = {
    println("X coordinate: ")
    val x =  scala.io.StdIn.readLine()
    println("Y coordinate: ")
    val y =  scala.io.StdIn.readLine()

    if(revealedCells.contains((y.toInt,x.toInt)) || !allCoords.contains((y.toInt,x.toInt)))
      getInput(revealedCells,allCoords)
    else
      (y.toInt,x.toInt)
  }

  private def getRevealedCells(grid: Grid): List[Coord] = {
    grid.toList.filter(row => row._2.reveal).map(_._1)
  }

  private def checkForBomb(entry: Coord, grid: Grid): Boolean = {
    grid(entry).value == "*"
  }

}
