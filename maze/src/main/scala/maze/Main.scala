package maze

object Main extends App {
  val size = 20
  val grid = Grid.makeGrid(size)

  val maze = DepthFirstSearch.generateMaze(grid, size-1)
  Grid.printGrid(maze, size)

}
