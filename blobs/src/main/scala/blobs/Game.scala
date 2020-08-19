package blobs

import scala.annotation.tailrec

class Game(coords: CoordWrapper, blobs: BlobWrapper) {

  def playBlobs(): Blob = {

    @tailrec
    def play(blobs: BlobWrapper): Blob = {
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

  def turn(blobs: BlobWrapper): BlobWrapper = {
    blobs match {
      case blobs: Blob2DWrapper =>
        val movableBlobs = Blobs2D.removeSmallestBlobs(blobs)
        val smallBlobs = Blobs2D.getSmallestBlobs(blobs)
        val movedBlobs = Blobs2D.moveBlobs(blobs, movableBlobs)
        Blobs2D.mergeBlobs(movedBlobs, smallBlobs).asInstanceOf[BlobWrapper]
      case blobs: Blob3DWrapper =>
        val movableBlobs = Blobs3D.removeSmallestBlobs(blobs)
        val smallBlobs = Blobs3D.getSmallestBlobs(blobs)
        val movedBlobs = Blobs3D.moveBlobs(blobs, movableBlobs)
        Blobs3D.mergeBlobs(movedBlobs, smallBlobs).asInstanceOf[BlobWrapper]
    }

  }

}
