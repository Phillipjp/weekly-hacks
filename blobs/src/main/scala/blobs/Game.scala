package blobs

import scala.annotation.tailrec

class Game[C <: Coord, B <: Blob[C]](coords: CoordWrapper[C], blobs: BlobWrapper[B,C]) {

  def playBlobs(): Blob[C] = {

    @tailrec
    def play(blobs: BlobWrapper[B,C]): Blob[C] = {
      (coords, blobs) match {
        case (coords: Coord2DWrapper, blobs: Blob2DWrapper) =>
          val playArea = PlayArea2D.makeBlobPlayArea(coords.coords, blobs.blobs)
          PlayArea2D.printBlobPlayArea(playArea)
        case (coords: Coord3DWrapper, blobs: Blob3DWrapper) =>
          val playArea = PlayArea3D.makeBlobPlayArea(coords.coords, blobs.blobs)
          PlayArea3D.printBlobPlayArea(playArea)
      }

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

  def turn(blobs: BlobWrapper[B,C]): BlobWrapper[B,C] = {
    blobs match {
      case blobs: Blob2DWrapper =>
        val movableBlobs = Blob2DWrapper.removeSmallestBlobs(blobs)
        val smallBlobs = Blob2DWrapper.getSmallestBlobs(blobs)
        val movedBlobs = Blob2DWrapper.moveBlobs(blobs, movableBlobs)
        Blob2DWrapper.mergeBlobs(movedBlobs, smallBlobs).asInstanceOf[BlobWrapper[B,C]]
      case blobs: Blob3DWrapper =>
        val movableBlobs = Blob3DWrapper.removeSmallestBlobs(blobs)
        val smallBlobs = Blob3DWrapper.getSmallestBlobs(blobs)
        val movedBlobs = Blob3DWrapper.moveBlobs(blobs, movableBlobs)
        Blob3DWrapper.mergeBlobs(movedBlobs, smallBlobs).asInstanceOf[BlobWrapper[B,C]]
    }

  }

  private[blobs] def removeSmallestBlobs(blobs: Seq[Blob[C]]): Seq[Blob[C]] =
    blobs.groupBy(_.size).toSeq.sortBy(_._1).tail.flatMap(_._2)

  private[blobs] def getSmallestBlobs(blobs: Seq[Blob[C]]): Seq[Blob[C]] =
    blobs.groupBy(_.size).toSeq.minBy(_._1)._2

  private[blobs] def moveBlobs(blobs: Seq[Blob[C]], movableBlobs: Seq[Blob[C]]): Seq[Blob[C]] = {
    movableBlobs.map { blob =>
      val validBlobs = blobs.filter(b => b != blob && b.size <= blob.size)
      val closestBlob = blob.findClosestBlob(validBlobs)
      blob.moveTowardsBlob(closestBlob)
    }
  }

//  private[blobs] def mergeBlobs(blobs: Seq[Blob[C]]): BlobWrapper[B,C] = {
//
//    blobs.groupBy(b => b.coord).map { case (_, clashBlobs) =>
//      val size = clashBlobs.map(_.size).sum
//      clashBlobs.head match {
//        case blob2D: Blob2D => blob2D.copy(size = size)
//        case blob3D: Blob3D => blob3D.copy(size = size)
//      }
//    }.toSeq
//  }

}
