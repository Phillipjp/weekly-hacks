package minesweeper

object Minesweeper extends App {

  val size = 10
  val numBombs = 12
  val grid = Grid.makeGrid(size, numBombs)
  val game = new Game(grid, size, numBombs)
  game.run()

}
