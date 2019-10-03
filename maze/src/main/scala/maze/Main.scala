package maze
import  maze.DepthFirstSearch._
object Main extends App {

  val size = 5
  val grid = Grid.makeGrid(size)

  val maze = DepthFirstSearch.generateMaze(grid)
  Grid.printGrid(maze, size)

}
