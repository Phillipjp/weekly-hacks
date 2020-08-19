package blobs

case class Blob2DWrapper(blobs: Seq[Blob2D]) extends BlobWrapper[Blob2D, Coord2D]

object Blob2DWrapper {
  def removeSmallestBlobs(blobs: Blob2DWrapper): Blob2DWrapper =
    Blob2DWrapper(blobs.blobs.groupBy(_.size).toSeq.sortBy(_._1).tail.flatMap(_._2))

  def getSmallestBlobs(blobs: Blob2DWrapper): Blob2DWrapper =
    Blob2DWrapper(blobs.blobs.groupBy(_.size).toSeq.minBy(_._1)._2)

  def moveBlobs(blobs: Blob2DWrapper, movableBlobs: Blob2DWrapper): Blob2DWrapper = {
    Blob2DWrapper(
      movableBlobs.blobs.map { blob =>
        val validBlobs = blobs.blobs.filter(b => b != blob && b.size <= blob.size)
        val closestBlob = blob.findClosestBlob(validBlobs)
        blob.moveTowardsBlob(closestBlob)
      }.asInstanceOf[Seq[Blob2D]]
    )
  }

  def mergeBlobs(movedBlobs: Blob2DWrapper, smallBlobs: Blob2DWrapper): Blob2DWrapper = {
    Blob2DWrapper(
      (movedBlobs.blobs ++ smallBlobs.blobs).groupBy(b => b.coord).map { case (_, clashBlobs) =>
        val size = clashBlobs.map(_.size).sum
        clashBlobs.head.copy(size = size)
      }.toSeq
    )
  }
}
