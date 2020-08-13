package blobs

object Main {


  def main(args: Array[String]): Unit = {

    // make blobs
    val blobs = Seq(Blob2D(Coords2D(4,4),1), Blob2D(Coords2D(4,3),2), Blob2D(Coords2D(1,1),3), Blob2D(Coords2D(1,2),4), Blob2D(Coords2D(2,1),2))

    Game.playBlobs(blobs)



  }
}
