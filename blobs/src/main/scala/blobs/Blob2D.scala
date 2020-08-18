package blobs
import scala.math.{abs, atan2}

case class Blob2D(coord: Coord2D, size: Int) extends Blob[Coord2D] {

  private def moveNorth: Blob2D = {
    this.copy(coord = this.coord.copy(y = this.coord.y + 1))
  }

  private def moveSouth: Blob2D = {
    this.copy(coord = this.coord.copy(y = this.coord.y - 1))
  }

  private def moveEast: Blob2D = {
    this.copy(coord = this.coord.copy(x = this.coord.x + 1))
  }

  private def moveWest: Blob2D = {
    this.copy(coord = this.coord.copy(x = this.coord.x - 1))
  }

  private def moveNorthEast: Blob2D = {
    this.moveNorth.moveEast
  }

  private def moveSouthEast: Blob2D = {
    this.moveSouth.moveEast
  }

  private def moveNorthWest: Blob2D = {
    this.moveNorth.moveWest
  }

  private def moveSouthWest: Blob2D = {
    this.moveSouth.moveWest
  }

  override def findClosestBlob(blobs: Seq[Blob[Coord2D]]): Blob[Coord2D] = {
    val size = blobs.flatMap{blob =>
      val x = abs(this.coord.x - blob.coord.x)
      val y = abs(this.coord.y - blob.coord.y)
      Seq(x,y)
    }.max*2

    val distancesFromBlob = distanceGrid(size)

    val closestBlobs = blobs.filter(_ != this).map(blob=>distancesFromBlob((blob.coord.x, blob.coord.y)) -> blob).groupBy(_._1).toSeq.minBy(_._1)._2.map(_._2)

    if(closestBlobs.size ==1){
      closestBlobs.head
    }
    else{
      breakDistanceTie(closestBlobs)
    }
  }

  def distanceGrid(size: Int): Map[(Int,Int), Int] = {

    (size to 0 by -1).filter(_%2==0).flatMap(size => makeValueGrid(size, size/2)).toMap
  }


  private [blobs] def makeValueGrid(size: Int, value: Int): Map[(Int, Int), Int] = {
    val startX = this.coord.x - (size/2)
    val startY = this.coord.y - (size/2)
    val endX = this.coord.x + (size/2)
    val endY = this.coord.y + (size/2)

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

  private def breakDistanceTie(blobs: Seq[Blob[Coord2D]]): Blob[Coord2D] = {
    blobs.map { blob =>
      atan2(blob.coord.y - this.coord.y, blob.coord.x - this.coord.x) -> blob
    }.minBy(_._1)._2
  }

  override def moveTowardsBlob(blob: Blob[Coord2D]): Blob[Coord2D] = {
    if(blob.coord.y > this.coord.y && blob.coord.x == this.coord.x)
      this.moveNorth
    else if(blob.coord.y < this.coord.y && blob.coord.x == this.coord.x)
      this.moveSouth
    else if(blob.coord.y == this.coord.y && blob.coord.x > this.coord.x)
      this.moveEast
    else if(blob.coord.y == this.coord.y && blob.coord.x < this.coord.x)
      this.moveWest
    else if(blob.coord.y > this.coord.y && blob.coord.x > this.coord.x)
      this.moveNorthEast
    else if(blob.coord.y < this.coord.y && blob.coord.x > this.coord.x)
      this.moveSouthEast
    else if(blob.coord.y < this.coord.y && blob.coord.x < this.coord.x)
      this.moveSouthWest
    else
      this.moveNorthWest
  }
}
