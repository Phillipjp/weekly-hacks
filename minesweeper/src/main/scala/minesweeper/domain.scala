package minesweeper

object domain {

  type Coord = (Int, Int)
  type Grid = Map[Coord, Cell]

}
