package minesweeper
import minesweeper.domain.{Coord, Grid}

import scala.annotation.tailrec
import scala.util.Random

case class Cell(value: String,  reveal: Boolean)

object Grid {

  def makeGrid(size: Int, numBombs: Int): Grid = {
    val start = (0,0)
    val g: Seq[Coord] = LazyList.iterate(start)(cell => nextCell(cell, size-1, 0)).takeWhile(_._1 < size)
    val bombLocations = makeBombLocations(g.toList, numBombs)
    val grid = g.toList
        .map{ key =>
          if(bombLocations.contains(key))
            (key, Cell("*", false))
          else
            (key, Cell("0", false))
        }
        .toMap

    updateCellsWithSurroundingBombs(grid)
  }

  private def nextCell(current: Coord, size: Int, xReset: Int): Coord = {
    if (current._2 < size)
      (current._1, current._2 + 1)
    else
      (current._1 + 1, xReset)
  }

  private def getNumberOfSurroundingBombs(location: Coord, grid: Grid): Int = {
    val surroundingLocations: Seq[(Int, Int)] = getSurroundingCellLocations(location)
    surroundingLocations
      .map { key =>
        grid.get(key) match{
          case Some(cell) => cell.value
          case _ => "0"
        }
      }.count(_ == "*")
  }

  private def getSurroundingCellLocations(location: Coord) = {
    val start = (location._1 - 1, location._2 - 1)
    LazyList.iterate(start)(cell => nextCell(cell, start._2 + 2, start._2)).takeWhile(_._1 < start._1 + 3).filter(_ != location).toList
  }

  private def makeBombLocations(grid: List[Coord], numBombs: Int): List[Coord] = {

    def generateLocations(grid: List[Coord], locations: List[Coord], numBombs: Int): List[Coord] = {
      if(locations.length == numBombs)
        locations
      else{
        val rand = Random
        val location = grid(rand.nextInt(grid.length))
        generateLocations(grid diff List(location), locations :+ location, numBombs)
      }
    }
    generateLocations(grid, List(), numBombs)
  }

  private def updateCellsWithSurroundingBombs(grid: Grid):Grid = {
    grid.toSeq
      .map{case(key, cell) =>
        if(cell.value == "*")
          (key, cell)
        else
          (key, Cell(getNumberOfSurroundingBombs(key, grid).toString, cell.reveal))
      }.toMap
  }

  @tailrec
  def revealCells(cellLocations: List[Coord], grid: Grid): Grid = {
    if(cellLocations.isEmpty)
      grid
    else{
      val location = cellLocations.head
      grid.get(location) match {
        case Some(cell) =>
          if (cell.reveal)
            revealCells(cellLocations.drop(1), grid)
          else {
            val newGrid = grid + (location -> Cell(cell.value, true))
            if (cell.value != "0")
              revealCells(cellLocations.drop(1), newGrid)
            else {
              val surroundingLocations = getSurroundingCellLocations(location).filter(grid.keySet.contains(_))
              val newCellLocations = (cellLocations.drop(1) ++ surroundingLocations).toSet.toList
              revealCells(newCellLocations, newGrid)
            }
          }
        case _ =>
          revealCells(cellLocations.drop(1), grid)

      }

    }
  }

  def printGrid(grid: Grid, size: Int): Unit = {

    print("    ")
    (0 until size).toList.foreach(c => print(" " + c.toString.charAt(0) + " "))
    println()
    print("    ")
    (0 until size).toList.foreach { c =>
      if (c.toString.length > 1)
        print(" " + c.toString.charAt(1) + " ")
      else
        print("   ")
    }
    println()
    print("    ")
    (0 until size*3).toList.foreach(c => print("-"))
    println()
    for (i <- 0 until size) {
      if(i < 10 )
        print(i + "  |")
      else
        print(i + " |")
      for (j <- 0 until size) {
        val cell = grid((i,j))
        if(cell.reveal)
          print(" " + cell.value + " ")
        else
          print(" . ")
      }
      println()
    }
  }
}
