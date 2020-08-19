package blobs

case class Blob3DWrapper(blobs: Seq[Blob3D]) extends BlobWrapper[Blob3D, Coord3D]

object Blob3DWrapper {
  def removeSmallestBlobs(blobs: Blob3DWrapper): Blob3DWrapper =
    Blob3DWrapper(blobs.blobs.groupBy(_.size).toSeq.sortBy(_._1).tail.flatMap(_._2))

  def getSmallestBlobs(blobs: Blob3DWrapper): Blob3DWrapper =
    Blob3DWrapper(blobs.blobs.groupBy(_.size).toSeq.minBy(_._1)._2)

  def moveBlobs(blobs: Blob3DWrapper, movableBlobs: Blob3DWrapper): Blob3DWrapper = {
    Blob3DWrapper(
      movableBlobs.blobs.map { blob =>
        val validBlobs = blobs.blobs.filter(b => b != blob && b.size <= blob.size)
        val closestBlob = blob.findClosestBlob(validBlobs)
        blob.moveTowardsBlob(closestBlob)
      }.asInstanceOf[Seq[Blob3D]]
    )
  }

  def mergeBlobs(movedBlobs: Blob3DWrapper, smallBlobs: Blob3DWrapper): Blob3DWrapper = {
    Blob3DWrapper(
      (movedBlobs.blobs ++ smallBlobs.blobs).groupBy(b => b.coord).map { case (_, clashBlobs) =>
        val size = clashBlobs.map(_.size).sum
        clashBlobs.head.copy(size = size)
      }.toSeq
    )
  }
}
