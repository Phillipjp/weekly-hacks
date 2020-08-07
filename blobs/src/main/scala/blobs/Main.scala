package blobs

object Main {

  def add(x: Int, y: Int): Int = {
    x + y
  }

  def main(args: Array[String]): Unit = {

    // make blobs
    val blobs = Seq(Blob(3,3,0),
      Blob(2,2,1), Blob(2,3,1), Blob(2,4,1), Blob(3,2,1), Blob(3,4,1), Blob(4,2,1), Blob(4,3,1), Blob(4,4,1),
      Blob(1,1,2), Blob(1,2,2), Blob(1,3,2), Blob(1,4,2), Blob(1,5,2), Blob(2,5,2), Blob(3,5,2), Blob(4,5,2), Blob(5,5,2), Blob(5,4,2), Blob(5,3,2), Blob(5,2,2), Blob(5,1,2), Blob(4,1,2), Blob(3,1,2), Blob(2,1,2),
      Blob(3,6,3), Blob(6,6,3))

    // make grid
    val grid = Grid.makeGridCoords(0,0,6,6)

    val gridWithBlobs = Grid.makeBlobGrid(grid, blobs)
    Grid.printGridWithBlobs(gridWithBlobs)


    val a = Map("A" -> 1)
    val b = Map("A" -> 2)
    println(a ++ b)
    // move blobs

    // combine if match



  }
}
