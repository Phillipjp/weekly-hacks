package minesweeper

object Minesweeper extends App {

  val size = 2
  val numBombs = 4
  val grid = Grid.makeGrid(size, numBombs)
  val game = new Game(grid, size, numBombs)
  game.run()

}
