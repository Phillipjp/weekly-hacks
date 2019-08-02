package minesweeper
object Minesweeper extends App {

  val size = 8
  val grid = Grid.makeGrid(size, 10)
  Grid.printGrid(grid, size)
  println(grid((2,6)))

  if(grid((2,6)).value == "*")
    println("BOMB")
  else {
    val newGrid = Grid.revealCell(List((2, 6)), grid)
    Grid.printGrid(newGrid, size)
  }
}
