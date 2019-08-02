import scala.util.Random

case class Cell(value: String,  reveal: Boolean)

object Grid {

  //Map[(Int, Int), Cell]
  def makeGrid(size: Int, numBombs: Int): Map[(Int, Int), Cell] = {
    val g: Seq[(Int, Int)] = LazyList.iterate((0,0))(cell => nextCell(cell, size-1, 0)).takeWhile(_._1 < size)
    val bombLocations = makeBombLocations(g.toList, numBombs)
    val grid = g.toList
        .map{ key =>
          if(bombLocations.contains(key))
            (key, Cell("*", true))
          else
            (key, Cell("0", true))
        }
        .toMap

    updateCellsWithSurroundingBombs(grid)
  }

  private def nextCell(current: (Int,Int), size: Int, xReset: Int): (Int, Int) = {
    if (current._2 < size)
      (current._1, current._2 + 1)
    else
      (current._1 + 1, xReset)
  }

  private def getNumberOfSurroundingBombs(location: (Int, Int), grid: Map[(Int, Int), Cell]): Int = {
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

  private def getSurroundingCellLocations(location: (Int, Int), start: (Int, Int)) = {
    LazyList.iterate(start)(cell => nextCell(cell, start._2 + 2, start._2)).takeWhile(_._1 < start._1 + 3).filter(_ != location).toList
  }

  private def makeBombLocations(grid: List[(Int, Int)], numBombs: Int): List[(Int,Int)] = {

    def generateLocations(grid: List[(Int, Int)], locations: List[(Int,Int)], numBombs: Int): List[(Int, Int)] = {
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

  private def updateCellsWithSurroundingBombs(grid: Map[(Int, Int), Cell]): Map[(Int, Int),Cell] = {
    grid.toSeq
      .map{case(key, cell) =>
        if(cell.value == "*")
          (key, cell)
        else
          (key, Cell(getNumberOfSurroundingBombs(key, grid).toString, cell.reveal))
      }.toMap
  }

  def revealCell(key: (Int,Int), grid: Map[(Int, Int), Cell]):  Map[(Int, Int), Cell]= {
    grid.get(key) match {
      case Some(cell) =>
        if(cell.reveal)
          grid
        else {
          val newGrid = grid + (key -> Cell(cell.value, true))
          if (cell.value != "0")
            newGrid
          else {
            val start = (key._1 - 1, key._2 - 1)
            val surroundingLocations = getSurroundingCellLocations(key, start).filter(grid.keySet.contains(_))
            val grids: Seq[Map[(Int, Int), Cell]] = surroundingLocations.map(location => revealCell(location, newGrid))
            mergeGrids(grids)
          }
        }


    }
  }


  private def mergeGrids(grids: Seq[Map[(Int, Int), Cell]]): Map[(Int, Int), Cell] ={
    val groupedCells: Map[(Int, Int), Seq[((Int, Int), Cell)]] = grids.flatMap(_.toSeq).groupBy(_._1)
    groupedCells.keySet.toSeq.map{ key =>
      val cells: Seq[Cell] = groupedCells(key).map(_._2)
      if(cells.exists(_.reveal)){
        (key, cells.filter(_.reveal).head)
      }
      else{
        (key, cells.head)
      }
    }
      .toMap
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

  private def printGrid(grid: Map[(Int, Int), Cell], size: Int): Unit = {
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
