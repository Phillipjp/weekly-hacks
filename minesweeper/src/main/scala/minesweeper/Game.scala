package minesweeper

import minesweeper.domain.{coord, grid}

class Game(grid: grid) {

  def run(): Unit = {

  }

  private def getInput(revealedCells: List[coord], allCoords: List[coord]): coord = {
    println("X coordinate: ")
    val x =  scala.io.StdIn.readLine().toInt
    println("Y coordinate: ")
    val y =  scala.io.StdIn.readLine().toInt

    if(revealedCells.contains((y,x)) || !allCoords.contains((y,x)))
      getInput(revealedCells,allCoords)
    else
      (y,x)


  }

  private def getRevealedCells(grid: grid): List[coord] = {
    grid.toList.filter(row => row._2.reveal).map(_._1)
  }

  private def checkForBomb(entry: coord, grid: grid): Boolean = {
    grid(entry).value == "*"
  }

}
