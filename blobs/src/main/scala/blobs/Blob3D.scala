package blobs
import scala.math.{abs, atan2}
case class Blob3D(coord: Coord3D, size: Int) extends Blob[Blob3D, Coord3D] {

  override def findClosestBlob(blobs: Seq[Blob[Blob3D, Coord3D]]): Blob[Blob3D, Coord3D] = {
    val size = blobs.flatMap{blob =>
      val x = abs(this.coord.x - blob.coord.x)
      val y = abs(this.coord.y - blob.coord.y)
      val z = abs(this.coord.z - blob.coord.z)
      Seq(x,y,z)
    }.max*2

    val distancesFromBlob = distanceCube(size)

    val closestBlobs = blobs.filter(_ != this).map(blob=>distancesFromBlob((blob.coord.x, blob.coord.y, blob.coord.z)) -> blob)
      .groupBy(_._1).toSeq.minBy(_._1)._2.map(_._2)

    if(closestBlobs.size ==1){
      closestBlobs.head
    }
    else{
      breakDistanceTie(closestBlobs)
    }
  }

  def distanceCube(size: Int): Map[(Int,Int,Int), Int] = {
    val minZ = this.coord.z - (size/2)
    val maxZ = this.coord.z + (size/2)
    val outerGrid =as2DBlob(this).distanceGrid(size)

    (maxZ to minZ by -1).flatMap{z =>
      val innerSize = (abs(z - this.coord.z)*2)+1
      val inner = innerDistanceSquare(innerSize, z)
      val outer = outerGrid.map{case((x,y), v) =>
        (x,y,z) -> v}
      outer ++ inner
    }.toMap

  }

  private [blobs] def innerDistanceSquare(size: Int, z: Int): Map[(Int,Int, Int), Int] = {
    val value = size/2

    val startX = this.coord.x - (size/2)
    val startY = this.coord.y - (size/2)
    val endX = this.coord.x + (size/2)
    val endY = this.coord.y + (size/2)

    val start = (startX, startY, z)

    def nextCell(current: (Int, Int, Int), endX: Int, xReset: Int): (Int, Int, Int) = {
      if (current._1 < endX)
        (current._1 + 1, current._2, z)
      else
        (xReset, current._2 + 1, z)
    }
    Stream.iterate(start)(cell => nextCell(cell, endX, startX)).takeWhile(curr => curr._1 <= endX && curr._2 <= endY).toList
      .map{case(x,y, z) => (x,y,z) -> value}.toMap
  }

  private [blobs] def breakDistanceTie(blobs: Seq[Blob[Blob3D, Coord3D]]): Blob[Blob3D, Coord3D] = {
    val this2D = (this.coord.x/this.coord.z, this.coord.y/this.coord.z)
    blobs.map{blob => atan2(blob.coord.y/blob.coord.z - this2D._2,  blob.coord.x/blob.coord.z - this2D._1) -> blob}
      .minBy(_._1)._2
  }

  override def moveTowardsBlob(blob: Blob[Blob3D, Coord3D]): Blob[Blob3D, Coord3D] = {
    if(blob.coord.z == this.coord.z){
      val blob2D = as2DBlob(this).moveTowardsBlob(as2DBlob(blob))
      this.copy(coord = this.coord.copy(x = blob2D.coord.x, y = blob2D.coord.y))
    }
    else if(blob.coord.x == this.coord.x && blob.coord.y == this.coord.y){
      if(blob.coord.z > this.coord.z)
        this.copy(coord = this.coord.copy(z = this.coord.z + 1))
      else
        this.copy(coord = this.coord.copy(z = this.coord.z - 1))
    }
    else if(blob.coord.z > this.coord.z){
      val blob2D = as2DBlob(this).moveTowardsBlob(as2DBlob(blob))
      this.copy(coord = this.coord.copy(x = blob2D.coord.x, y = blob2D.coord.y, z = this.coord.z + 1))
    }
    else{
      val blob2D = as2DBlob(this).moveTowardsBlob(as2DBlob(blob))
      this.copy(coord = this.coord.copy(x = blob2D.coord.x, y = blob2D.coord.y, z = this.coord.z - 1))
    }
  }

  private def as2DBlob(blob: Blob[Blob3D, Coord3D]): Blob2D = Blob2D(Coord2D(blob.coord.x, blob.coord.y), blob.size)
}
