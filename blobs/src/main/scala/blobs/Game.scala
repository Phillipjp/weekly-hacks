package blobs

object Game {

  def playBlobs(blobs: Seq[Blob2D]): Blob2D = {

    val coords = Grid.makeGridCoords(0,0,4,4)

    def play(blobs: Seq[Blob2D]): Blob2D = {
      val gridWithBlobs = Grid.makeBlobGrid(coords, blobs)
      Grid.printGridWithBlobs(gridWithBlobs)
      if (blobs.size == 1) {
        blobs.head
      }
      else {
        val blobsAfterTurn = turn(blobs)
        play(blobsAfterTurn)
      }
    }

    play(blobs)
  }

  def turn(blobs: Seq[Blob2D]): Seq[Blob2D] = {
    val movableBlobs = removeSmallestBlobs(blobs)
    val smallBlobs = getSmallestBlobs(blobs)
    val movedBlobs = moveBlobs(blobs, movableBlobs)
    mergeBlobs(movedBlobs ++ smallBlobs)
  }

  private[blobs] def removeSmallestBlobs(blobs: Seq[Blob2D]): Seq[Blob2D] =
    blobs.groupBy(_.size).toSeq.sortBy(_._1).tail.flatMap(_._2)

  private[blobs] def getSmallestBlobs(blobs: Seq[Blob2D]): Seq[Blob2D] =
    blobs.groupBy(_.size).toSeq.minBy(_._1)._2

  private[blobs] def moveBlobs(blobs: Seq[Blob2D], movableBlobs: Seq[Blob2D]): Seq[Blob2D] = {
    movableBlobs.map { blob =>
      val validBlobs = blobs.filter(_ != blob).filter(_.size <= blob.size)
      val closestBlob = blob.findClosestBlob(validBlobs)
      blob.moveTowardsBlob(closestBlob)
    }
  }

  private[blobs] def mergeBlobs(blobs: Seq[Blob2D]): Seq[Blob2D] = {
    blobs.groupBy(b => b.coord).map { case (_, clashBlobs) =>

      val size = clashBlobs.map(_.size).sum
      clashBlobs.head.copy(size = size)
    }.toSeq
  }

}
