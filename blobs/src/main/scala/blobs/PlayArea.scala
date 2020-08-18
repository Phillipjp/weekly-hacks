package blobs

trait PlayArea [C <: Coord, B <: Blob[C]]{

  def makePlayAreaCoords(startCoord: C, endCoord: C): Seq[C]

  def makeBlobPlayArea(coords: Seq[C], blobs: Seq[B]): Map[C, Seq[B]]

  def printBlobPlayArea(playArea:  Map[C, Seq[B]]): Unit
}
