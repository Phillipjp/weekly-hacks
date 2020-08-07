package blobs


object Grid {

  def makeGridCoords(startX: Int, startY: Int, endX: Int, endY: Int): List[(Int, Int)] = {
    val start = (startX, startY)
    Stream.iterate(start)(cell => nextCell(cell, endX, startX)).takeWhile(curr => curr._1 <= endX && curr._2 <= endY).toList

  }

  private def nextCell(current: (Int, Int), endX: Int, xReset: Int): (Int, Int) = {
    if (current._1 < endX)
      (current._1 + 1, current._2)
    else
      (xReset, current._2 + 1)
  }

  def makeBlobGrid(emptyGrid: List[(Int, Int)], blobs: Seq[Blob]): Map[(Int, Int), Seq[Blob]] = {
    val groupedBlobs: Map[(Int, Int), Seq[Blob]] = blobs.groupBy(blob => (blob.x, blob.y))

    emptyGrid.map{coord =>
      coord -> groupedBlobs.getOrElse(coord, Seq())
    }.toMap
  }

  def printGridWithBlobs(grid: Map[(Int, Int), Seq[Blob]]): Unit = {
    val xCoords = grid.keys.map(_._1)
    val maxX = xCoords.max
    val minX = xCoords.min

    val yCoords = grid.keys.map(_._2)
    val maxY = yCoords.max
    val minY = yCoords.min

    for(y <- maxY to minY by -1){
      for(x <- minX to maxX ){
        val blobs = grid((x,y))
        if(blobs.isEmpty){
          print(". ")
        }
        else{
          print(s"${blobs.head.size} ")
        }
      }
      println()
    }



  }



}
