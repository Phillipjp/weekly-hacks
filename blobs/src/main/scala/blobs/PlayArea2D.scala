package blobs


object PlayArea2D extends PlayArea[Coord2D, Blob2D] {

  override def makePlayAreaCoords(startCoord: Coord2D, endCoord: Coord2D): List[Coord2D] = {

    def nextCell(current: Coord2D, endX: Int, xReset: Int): Coord2D = {
      if (current.x < endX) {
        current.copy(x = current.x + 1)
      } else
        Coord2D(xReset, current.y + 1)
    }

    Stream.iterate(startCoord)(cell => nextCell(cell, endCoord.x, startCoord.x)).takeWhile(curr => curr.x <= endCoord.x && curr.y <= endCoord.y).toList
  }



  override def makeBlobPlayArea(coords: Seq[Coord2D], blobs: Seq[Blob2D]): Map[Coord2D, Seq[Blob2D]] = {
    val groupedBlobs: Map[Coord2D, Seq[Blob2D]] = blobs.groupBy(blob => blob.coord)

    coords.map{ coord =>
      coord -> groupedBlobs.getOrElse(coord, Seq())
    }.toMap
  }

  override def printBlobPlayArea(playArea: Map[Coord2D, Seq[Blob2D]]): Unit = {
    val xCoords = playArea.keys.map(_.x)
    val maxX = xCoords.max
    val minX = xCoords.min

    val yCoords = playArea.keys.map(_.y)
    val maxY = yCoords.max
    val minY = yCoords.min

    for(y <- maxY to minY by -1){
      for(x <- minX to maxX ){
        val blobs = playArea(Coord2D(x,y))
        if(blobs.isEmpty){
          print(". ")
        }
        else{
          print(s"${blobs.head.size} ")
        }
      }
      println()
    }
    println()
  }

}
