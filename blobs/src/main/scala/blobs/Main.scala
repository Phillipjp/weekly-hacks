package blobs

object Main {


  def main(args: Array[String]): Unit = {

    // make blobs
    val blobs = Seq(Blob(4,4,1), Blob(4,3,2), Blob(1,1,3), Blob(1,2,4), Blob(2,1,2))

    // make grid
    val coords = Grid.makeGridCoords(0,0,4,4)

    Game.playBlobs(blobs)



  }
}
