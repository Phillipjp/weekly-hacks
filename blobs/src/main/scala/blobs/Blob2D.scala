package blobs
import scala.math.{abs, atan2}

case class Blob2D(coord: Coord2D, size: Int) extends Blob

object Blob2D{

  private def moveNorth(blob: Blob2D): Blob2D = {
    blob.copy(coord = blob.coord.copy(y = blob.coord.y + 1))
  }

  private def moveSouth(blob: Blob2D): Blob2D = {
    blob.copy(coord = blob.coord.copy(y = blob.coord.y - 1))
  }

  private def moveEast(blob: Blob2D): Blob2D = {
    blob.copy(coord = blob.coord.copy(x = blob.coord.x + 1))
  }

  private def moveWest(blob: Blob2D): Blob2D = {
    blob.copy(coord = blob.coord.copy(x = blob.coord.x - 1))
  }

  private def moveNorthEast(blob: Blob2D): Blob2D = {
    moveEast(moveNorth(blob))
  }

  private def moveSouthEast(blob: Blob2D): Blob2D = {
    moveEast(moveSouth(blob))
  }

  private def moveNorthWest(blob: Blob2D): Blob2D = {
    moveWest(moveNorth(blob))
  }

  private def moveSouthWest(blob: Blob2D): Blob2D = {
    moveWest(moveSouth(blob))
  }

  def findClosestBlob(blob: Blob2D, blobs: Seq[Blob2D]): Blob2D = {
    val size = blobs.flatMap{b =>
      val x = abs(blob.coord.x - b.coord.x)
      val y = abs(blob.coord.y - b.coord.y)
      Seq(x,y)
    }.max*2

    val distancesFromBlob = distanceGrid(blob, size)

    val closestBlobs = blobs.filter(_ != blob).map(b=>distancesFromBlob((b.coord.x, b.coord.y)) -> b).groupBy(_._1).toSeq.minBy(_._1)._2.map(_._2)

    if(closestBlobs.size ==1){
      closestBlobs.head
    }
    else{
      breakDistanceTie(blob, closestBlobs)
    }
  }

  def distanceGrid(blob: Blob2D, size: Int): Map[(Int,Int), Int] = {

    (size to 0 by -1).filter(_%2==0).flatMap(size => makeValueGrid(blob, size, size/2)).toMap
  }


  private [blobs] def makeValueGrid(blob: Blob2D, size: Int, value: Int): Map[(Int, Int), Int] = {
    val startX = blob.coord.x - (size/2)
    val startY = blob.coord.y - (size/2)
    val endX = blob.coord.x + (size/2)
    val endY = blob.coord.y + (size/2)

    val start = (startX, startY, value)

    def nextCell(current: (Int, Int, Int), endX: Int, xReset: Int): (Int, Int, Int) = {
      if (current._1 < endX)
        (current._1 + 1, current._2, value)
      else
        (xReset, current._2 + 1, value)
    }
    Stream.iterate(start)(cell => nextCell(cell, endX, startX)).takeWhile(curr => curr._1 <= endX && curr._2 <= endY).toList
      .map{case(x,y, v) => (x,y) -> v}.toMap

  }

  private def breakDistanceTie(blob: Blob2D, blobs: Seq[Blob2D]): Blob2D = {
    blobs.map { b =>
      atan2(b.coord.y - blob.coord.y, b.coord.x - blob.coord.x) -> b
    }.minBy(_._1)._2
  }

  def moveTowardsBlob(blobA: Blob2D, blobB: Blob2D): Blob2D = {
    if(blobB.coord.y > blobA.coord.y && blobB.coord.x == blobA.coord.x)
      this.moveNorth(blobA)
    else if(blobB.coord.y < blobA.coord.y && blobB.coord.x == blobA.coord.x)
      this.moveSouth(blobA)
    else if(blobB.coord.y == blobA.coord.y && blobB.coord.x > blobA.coord.x)
      this.moveEast(blobA)
    else if(blobB.coord.y == blobA.coord.y && blobB.coord.x < blobA.coord.x)
      this.moveWest(blobA)
    else if(blobB.coord.y > blobA.coord.y && blobB.coord.x > blobA.coord.x)
      this.moveNorthEast(blobA)
    else if(blobB.coord.y < blobA.coord.y && blobB.coord.x > blobA.coord.x)
      this.moveSouthEast(blobA)
    else if(blobB.coord.y < blobA.coord.y && blobB.coord.x < blobA.coord.x)
      this.moveSouthWest(blobA)
    else
      this.moveNorthWest(blobA)
  }

}
