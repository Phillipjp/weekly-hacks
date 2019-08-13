package minesweeper

import minesweeper.domain.{coord, grid}

import scala.annotation.tailrec

class Game(grid: grid, size: Int, numBombs: Int) {

  def run(): Unit = {
    val finalGrid = playGame(grid)
    Grid.printGrid(finalGrid, size)
  }


  @tailrec
  final def playGame(grid: grid): grid = {
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
  private def getInput(revealedCells: List[coord], allCoords: List[coord]): coord = {
    println("X coordinate: ")
    val x =  scala.io.StdIn.readLine()
    println("Y coordinate: ")
    val y =  scala.io.StdIn.readLine()

    if(revealedCells.contains((y.toInt,x.toInt)) || !allCoords.contains((y.toInt,x.toInt)))
      getInput(revealedCells,allCoords)
    else
      (y.toInt,x.toInt)
  }

  private def getRevealedCells(grid: grid): List[coord] = {
    grid.toList.filter(row => row._2.reveal).map(_._1)
  }

  private def checkForBomb(entry: coord, grid: grid): Boolean = {
    grid(entry).value == "*"
  }

}
