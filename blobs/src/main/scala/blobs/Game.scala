package blobs

import scala.annotation.tailrec

class Game[C <: Coord, B <: Blob[B, C]](coords: Seq[Coord], blobs: Seq[Blob[B,C]]) {

  def playBlobs(): Blob[B,C] = {

    @tailrec
    def play(blobs: Seq[Blob[B,C]]): Blob[B,C] = {
      (coords.head, blobs.head) match {
        case (_: Coord2D, _: Blob2D) =>
          val playArea = PlayArea2D.makeBlobPlayArea(coords.asInstanceOf[Seq[Coord2D]], blobs.asInstanceOf[Seq[Blob2D]])
          PlayArea2D.printBlobPlayArea(playArea)
        case (_: Coord3D, _: Blob3D) =>
          val playArea = PlayArea3D.makeBlobPlayArea(coords.asInstanceOf[Seq[Coord3D]], blobs.asInstanceOf[Seq[Blob3D]])
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

  def turn(blobs: Seq[Blob[B,C]]): Seq[Blob[B,C]] = {
    val movableBlobs = removeSmallestBlobs(blobs)
    val smallBlobs = getSmallestBlobs(blobs)
    val movedBlobs = moveBlobs(blobs, movableBlobs)
    mergeBlobs(movedBlobs ++ smallBlobs)
  }

  private[blobs] def removeSmallestBlobs(blobs: Seq[Blob[B,C]]): Seq[Blob[B,C]] =
    blobs.groupBy(_.size).toSeq.sortBy(_._1).tail.flatMap(_._2)

  private[blobs] def getSmallestBlobs(blobs: Seq[Blob[B,C]]): Seq[Blob[B,C]] =
    blobs.groupBy(_.size).toSeq.minBy(_._1)._2

  private[blobs] def moveBlobs(blobs: Seq[Blob[B,C]], movableBlobs: Seq[Blob[B,C]]): Seq[Blob[B,C]] = {
    movableBlobs.map { blob =>
      val validBlobs = blobs.filter(b => b != blob && b.size <= blob.size)
      val closestBlob = blob.findClosestBlob(validBlobs)
      blob.moveTowardsBlob(closestBlob)
    }
  }

  private[blobs] def mergeBlobs(blobs: Seq[Blob[B,C]]): Seq[Blob[B,C]] = {
    blobs.groupBy(b => b.coord).map { case (_, clashBlobs) =>

      val size = clashBlobs.map(_.size).sum
      clashBlobs.head match {
        case blob2D: Blob2D => blob2D.copy(size = size)
        case blob3D: Blob3D => blob3D.copy(size = size)
      }
    }.toSeq.asInstanceOf[Seq[Blob[B,C]]]
  }

}
