package minesweeper

object Minesweeper extends App {

  val size = 8
  val numBombs = 10
  val grid = Grid.makeGrid(size, numBombs)
  val game = new Game(grid, size, numBombs)
  game.run()

}
