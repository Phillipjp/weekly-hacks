package blobs

import scala.annotation.tailrec

class Game[C <: Coord, B <: Blob[B, C]](coords: Seq[C], blobs: Seq[B]) {

  def playBlobs(): B = {

    @tailrec
    def play(blobs: Seq[B]): B = {
      (coords, blobs) match {
        case (coords2D: Seq[Coord2D], blobs2D: Seq[Blob2D]) =>
          val playArea = PlayArea2D.makeBlobPlayArea(coords2D, blobs2D)
          PlayArea2D.printBlobPlayArea(playArea)
        case (coords3D: Seq[Coord3D], blobs3D: Seq[Blob3D]) =>
          val playArea = PlayArea3D.makeBlobPlayArea(coords3D, blobs3D)
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

  def turn(blobs: Seq[B]): Seq[B] = {
    val movableBlobs = removeSmallestBlobs(blobs)
    val smallBlobs = getSmallestBlobs(blobs)
    val movedBlobs = moveBlobs(blobs, movableBlobs)
    mergeBlobs(movedBlobs ++ smallBlobs)
  }

  private[blobs] def removeSmallestBlobs(blobs: Seq[B]): Seq[B] =
    blobs.groupBy(_.size).toSeq.sortBy(_._1).tail.flatMap(_._2)

  private[blobs] def getSmallestBlobs(blobs: Seq[B]): Seq[B] =
    blobs.groupBy(_.size).toSeq.minBy(_._1)._2

  private[blobs] def moveBlobs(blobs: Seq[B], movableBlobs: Seq[B]): Seq[B] = {
    movableBlobs.map { blob =>
      val validBlobs = blobs.filter(b => b != blob && b.size <= blob.size)
      val closestBlob = blob.findClosestBlob(validBlobs)
      blob.moveTowardsBlob(closestBlob)
    }
  }

  private[blobs] def mergeBlobs(blobs: Seq[B]): Seq[B] = {
    blobs.groupBy(b => b.coord).map { case (_, clashBlobs) =>

      val size = clashBlobs.map(_.size).sum
      clashBlobs.head match {
        case blob2D: Blob2D => blob2D.copy(size = size)
        case blob3D: Blob3D => blob3D.copy(size = size)
      }
    }.toSeq.asInstanceOf[Seq[B]]
  }

}
