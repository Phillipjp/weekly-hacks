package blobs

object Main {


  def main(args: Array[String]): Unit = {

    // 2D
    val blobs2D = Seq(Blob2D(Coord2D(4,4),1), Blob2D(Coord2D(4,3),2), Blob2D(Coord2D(1,1),3), Blob2D(Coord2D(1,2),4), Blob2D(Coord2D(2,1),2))
    val coords2D = PlayArea2D.makePlayAreaCoords(Coord2D(0,0), Coord2D(4,4))
    new Game(coords2D, blobs2D).playBlobs()

    val blobs3D: Seq[Blob3D] = Seq(Blob3D(Coord3D(2,1,1),1),Blob3D(Coord3D(0,0,0),2))
    val coords3D: Seq[Coord3D] = PlayArea3D.makePlayAreaCoords(Coord3D(0,0,0), Coord3D(2,1,1))
    new Game(coords3D, blobs3D).playBlobs()



  }
}
