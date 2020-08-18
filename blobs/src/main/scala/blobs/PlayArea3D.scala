package blobs

object PlayArea3D extends PlayArea[Coord3D, Blob3D] {

  override def makePlayAreaCoords(startCoord: Coord3D, endCoord: Coord3D): Seq[Coord3D] = {
    val start2D = Coord2D(startCoord.x, startCoord.y)
    val end2D = Coord2D(endCoord.x, endCoord.y)
    val grid = PlayArea2D.makePlayAreaCoords(start2D, end2D)
    (startCoord.z to endCoord.z).flatMap(z => grid.map{coord2D => Coord3D(coord2D.x,coord2D.y,z)})
  }

  override def makeBlobPlayArea(coords: Seq[Coord3D], blobs: Seq[Blob3D]): Map[Coord3D, Seq[Blob3D]] = {
    val groupedBlobs: Map[Coord3D, Seq[Blob3D]] = blobs.groupBy(blob => blob.coord)

    coords.map{ coord =>
      coord -> groupedBlobs.getOrElse(coord, Seq())
    }.toMap
  }

  override def printBlobPlayArea(playArea: Map[Coord3D, Seq[Blob3D]]): Unit = {
    val xCoords = playArea.keys.map(_.x)
    val maxX = xCoords.max
    val minX = xCoords.min

    val yCoords = playArea.keys.map(_.y)
    val maxY = yCoords.max
    val minY = yCoords.min

    val zCoords = playArea.keys.map(_.z)
    val maxZ = zCoords.max
    val minZ = zCoords.min

    for(z <- maxZ to minZ by -1) {
      for (y <- maxY to minY by -1) {
        for (x <- minX to maxX) {
          val blobs = playArea(Coord3D(x, y, z))
          if (blobs.isEmpty) {
            print(". ")
          }
          else {
            print(s"${blobs.head.size} ")
          }
        }
        println()
      }
      if(z != minZ)
        println("/")
    }
    println()
  }

}
