package minesweeper
import minesweeper.domain.{coord, grid}

import scala.annotation.tailrec
import scala.util.Random

case class Cell(value: String,  reveal: Boolean)

object Grid {

  //Map[(Int, Int), Cell]
  def makeGrid(size: Int, numBombs: Int): grid = {
    val g: Seq[coord] = LazyList.iterate((0,0))(cell => nextCell(cell, size-1, 0)).takeWhile(_._1 < size)
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

  private def nextCell(current: coord, size: Int, xReset: Int): coord = {
    if (current._2 < size)
      (current._1, current._2 + 1)
    else
      (current._1 + 1, xReset)
  }

  private def getNumberOfSurroundingBombs(location: coord, grid: grid): Int = {
    val start = (location._1 - 1, location._2 - 1)
    val surroundingLocations: Seq[(Int, Int)] = getSurroundingCellLocations(location, start)
    surroundingLocations
      .map { key =>
        grid.get(key) match{
          case Some(cell) => cell.value
          case _ => "0"
        }
      }.count(_ == "*")
  }

  private def getSurroundingCellLocations(location: coord, start: coord) = {
    LazyList.iterate(start)(cell => nextCell(cell, start._2 + 2, start._2)).takeWhile(_._1 < start._1 + 3).filter(_ != location).toList
  }

  private def makeBombLocations(grid: List[coord], numBombs: Int): List[coord] = {

    def generateLocations(grid: List[coord], locations: List[coord], numBombs: Int): List[coord] = {
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

  private def updateCellsWithSurroundingBombs(grid: grid):grid = {
    grid.toSeq
      .map{case(key, cell) =>
        if(cell.value == "*")
          (key, cell)
        else
          (key, Cell(getNumberOfSurroundingBombs(key, grid).toString, cell.reveal))
      }.toMap
  }

  @tailrec
  def revealCell(cellLocations: List[coord], grid: grid): grid = {
    if(cellLocations.isEmpty)
      grid
    else{
      val location = cellLocations.head
      grid.get(location) match {
        case Some(cell) =>
          if (cell.reveal)
            revealCell(cellLocations.drop(1), grid)
          else {
            val newGrid = grid + (location -> Cell(cell.value, true))
            if (cell.value != "0")
              revealCell(cellLocations.drop(1), newGrid)
            else {
              val start = (location._1 - 1, location._2 - 1)
              val surroundingLocations = getSurroundingCellLocations(location, start).filter(grid.keySet.contains(_))
              val newCellLocations = (cellLocations.drop(1) ++ surroundingLocations).toSet.toList
              revealCell(newCellLocations, newGrid)
            }
          }
        case _ =>
          revealCell(cellLocations.drop(1), grid)

      }

    }
  }

  def main(args: Array[String]): Unit = {
    val size = 10
    val grid = makeGrid(size, 20)
    printGrid(grid, size)

//    grid.get((7,7)) match {
//      case Some(value) => println(value)
//      case _ => println("no such key")
//
//    }

  }

  def printGrid(grid: grid, size: Int): Unit = {
    print("   ")
    (0 until size).toList.foreach(c => print(" " + c + " "))
    println()
    print("   ")
    (0 until size*3).toList.foreach(c => print("-"))
    println()
    for (i <- 0 until size) {
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
