package blobs

object Blobs{

  def removeSmallestBlobs[B <: Blob](blobs: Seq[B]): Seq[B] =
    blobs.groupBy(_.size).toSeq.sortBy(_._1).tail.flatMap(_._2)

  def getSmallestBlobs[B <: Blob](blobs: Seq[B]): Seq[B] =
    blobs.groupBy(_.size).toSeq.minBy(_._1)._2

  def moveBlobs[B <: Blob](blobs: Seq[B], movableBlobs: Seq[B], findClosestBlob:(B, Seq[B]) => B, moveTowardsBlob:(B, B) => B): Seq[B] = {
      movableBlobs.map { blob =>
        val validBlobs = blobs.filter(b => b != blob && b.size <= blob.size)
        val closestBlob = findClosestBlob(blob, validBlobs)
        moveTowardsBlob(blob, closestBlob)
      }
  }

  def mergeBlobs[B <: Blob](movedBlobs: Seq[B], smallBlobs: Seq[B]): Seq[B] = {
    (movedBlobs ++ smallBlobs).groupBy(b => b.coord).map { case (_, clashBlobs) =>
      val size = clashBlobs.map(_.size).sum
      clashBlobs.head match {
        case blob: Blob2D => blob.copy(size = size)
        case blob: Blob3D => blob.copy(size = size)
      }
    }.toSeq.asInstanceOf[Seq[B]]
  }

}

object Blobs2D {

  def removeSmallestBlobs(blobs: Blob2DWrapper): Blob2DWrapper =
    Blob2DWrapper(Blobs.removeSmallestBlobs(blobs.blobs))

  def getSmallestBlobs(blobs: Blob2DWrapper): Blob2DWrapper =
    Blob2DWrapper(Blobs.getSmallestBlobs(blobs.blobs))

  def moveBlobs(blobs: Blob2DWrapper, movableBlobs: Blob2DWrapper): Blob2DWrapper =
    Blob2DWrapper(Blobs.moveBlobs(blobs.blobs, movableBlobs.blobs, Blob2D.findClosestBlob, Blob2D.moveTowardsBlob))


  def mergeBlobs(movedBlobs: Blob2DWrapper, smallBlobs: Blob2DWrapper): Blob2DWrapper =
    Blob2DWrapper(Blobs.mergeBlobs(movedBlobs.blobs, smallBlobs.blobs))

}

object Blobs3D {

  def removeSmallestBlobs(blobs: Blob3DWrapper): Blob3DWrapper =
    Blob3DWrapper(Blobs.removeSmallestBlobs(blobs.blobs))

  def getSmallestBlobs(blobs: Blob3DWrapper): Blob3DWrapper =
    Blob3DWrapper(Blobs.getSmallestBlobs(blobs.blobs))

  def moveBlobs(blobs: Blob3DWrapper, movableBlobs: Blob3DWrapper): Blob3DWrapper =
    Blob3DWrapper(Blobs.moveBlobs(blobs.blobs, movableBlobs.blobs, Blob3D.findClosestBlob, Blob3D.moveTowardsBlob))


  def mergeBlobs(movedBlobs: Blob3DWrapper, smallBlobs: Blob3DWrapper): Blob3DWrapper =
    Blob3DWrapper(Blobs.mergeBlobs(movedBlobs.blobs, smallBlobs.blobs))

}
