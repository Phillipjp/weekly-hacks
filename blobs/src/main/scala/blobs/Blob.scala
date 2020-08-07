package blobs
import scala.math.{abs, atan2, pow, sqrt}

case class Blob(x: Int, y: Int, size: Int){

  def add(that: Blob): Blob = {
    this.copy(size = this.size + that.size)
  }

  def moveNorth: Blob = {
    this.copy(y = this.y + 1)
  }

  def moveSouth: Blob = {
    this.copy(y = this.y - 1)
  }

  def moveEast: Blob = {
    this.copy(x = this.x + 1)
  }

  def moveWest: Blob = {
    this.copy(x = this.x - 1)
  }

  def moveNorthEast: Blob = {
    this.moveNorth.moveEast
  }

  def moveSouthEast: Blob = {
    this.moveSouth.moveEast
  }

  def moveNorthWest: Blob = {
    this.moveNorth.moveWest
  }

  def moveSouthWest: Blob = {
    this.moveSouth.moveWest
  }

  def findClosestBlob(blobs: Seq[Blob]): Blob = {
    val size = blobs.flatMap{blob =>
      val x = abs(this.x - blob.x)
      val y = abs(this.y - blob.y)
      Seq(x,y)
    }.max*2

    val distancesFromBlob = distanceGrid(size)

    val closestBlobs = blobs.filter(_ != this).map(blob=>distancesFromBlob((blob.x, blob.y)) -> blob).groupBy(_._1).toSeq.minBy(_._1)._2.map(_._2)

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


  private def makeValueGrid(size: Int, value: Int): Map[(Int, Int), Int] = {
    val startX = this.x - (size/2)
    val startY = this.y - (size/2)
    val endX = this.x + (size/2)
    val endY = this.y + (size/2)

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

  private def breakDistanceTie(blobs: Seq[Blob]): Blob = {
    blobs.map { blob =>
      atan2(blob.y - this.y, blob.x - this.x) -> blob
    }.minBy(_._1)._2
  }

  def moveTowardsBlob(blob: Blob): Blob = {
    if(blob.y > this.y && blob.x == this.x)
      this.moveNorth
    else if(blob.y < this.y && blob.x == this.x)
      this.moveSouth
    else if(blob.y == this.y && blob.x > this.x)
      this.moveEast
    else if(blob.y == this.y && blob.x < this.x)
      this.moveWest
    else if(blob.y > this.y && blob.x > this.x)
      this.moveNorthEast
    else if(blob.y < this.y && blob.x > this.x)
      this.moveSouthEast
    else if(blob.y < this.y && blob.x < this.x)
      this.moveSouthWest
    else
      this.moveNorthWest
  }



}
