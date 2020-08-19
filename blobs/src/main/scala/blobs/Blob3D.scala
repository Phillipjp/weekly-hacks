package blobs
import scala.math.{abs, atan2}
case class Blob3D(coord: Coord3D, size: Int) extends Blob

object Blob3D{
  def findClosestBlob(blob: Blob3D, blobs: Seq[Blob3D]): Blob3D = {
    val size = blobs.flatMap{b =>
      val x = abs(blob.coord.x - b.coord.x)
      val y = abs(blob.coord.y - b.coord.y)
      val z = abs(blob.coord.z - b.coord.z)
      Seq(x,y,z)
    }.max*2

    val distancesFromBlob = distanceCube(blob, size)

    val closestBlobs = blobs.filter(_ != blob).map(b=>distancesFromBlob((b.coord.x, b.coord.y, b.coord.z)) -> b)
      .groupBy(_._1).toSeq.minBy(_._1)._2.map(_._2)

    if(closestBlobs.size ==1){
      closestBlobs.head
    }
    else{
      breakDistanceTie(blob, closestBlobs)
    }
  }

  def distanceCube(blob: Blob3D, size: Int): Map[(Int,Int,Int), Int] = {
    val minZ = blob.coord.z - (size/2)
    val maxZ = blob.coord.z + (size/2)
    val outerGrid = Blob2D.distanceGrid(as2DBlob(blob), size)

    (maxZ to minZ by -1).flatMap{z =>
      val innerSize = (abs(z - blob.coord.z)*2)+1
      val inner = innerDistanceSquare(blob, innerSize, z)
      val outer = outerGrid.map{case((x,y), v) =>
        (x,y,z) -> v}
      outer ++ inner
    }.toMap

  }

  private [blobs] def innerDistanceSquare(blob: Blob3D, size: Int, z: Int): Map[(Int,Int, Int), Int] = {
    val value = size/2

    val startX = blob.coord.x - (size/2)
    val startY = blob.coord.y - (size/2)
    val endX = blob.coord.x + (size/2)
    val endY = blob.coord.y + (size/2)

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

  private [blobs] def breakDistanceTie(blob: Blob3D, blobs: Seq[Blob3D]): Blob3D = {
    val this2D = (blob.coord.x/blob.coord.z, blob.coord.y/blob.coord.z)
    blobs.map{ b => atan2(b.coord.y/b.coord.z - this2D._2,  b.coord.x/b.coord.z - this2D._1) -> b}
      .minBy(_._1)._2
  }

  def moveTowardsBlob(blobA: Blob3D, blobB: Blob3D): Blob3D = {
    if(blobB.coord.z == blobA.coord.z){
      val movedBlob = Blob2D.moveTowardsBlob(as2DBlob(blobA), as2DBlob(blobB))
      blobA.copy(coord = blobA.coord.copy(x = movedBlob.coord.x, y = movedBlob.coord.y))
    }
    else if(blobB.coord.x == blobA.coord.x && blobB.coord.y == blobA.coord.y){
      if(blobB.coord.z > blobA.coord.z)
        blobA.copy(coord = blobA.coord.copy(z = blobA.coord.z + 1))
      else
        blobA.copy(coord = blobA.coord.copy(z = blobA.coord.z - 1))
    }
    else if(blobB.coord.z > blobA.coord.z){
      val movedBlob = Blob2D.moveTowardsBlob(as2DBlob(blobA), as2DBlob(blobB))
      blobA.copy(coord = blobA.coord.copy(x = movedBlob.coord.x, y = movedBlob.coord.y, z = blobA.coord.z + 1))
    }
    else{
      val movedBlob = Blob2D.moveTowardsBlob(as2DBlob(blobA), as2DBlob(blobB))
      blobA.copy(coord = blobA.coord.copy(x = movedBlob.coord.x, y = movedBlob.coord.y, z = blobA.coord.z - 1))
    }
  }

  private def as2DBlob(blob: Blob3D): Blob2D = Blob2D(Coord2D(blob.coord.x, blob.coord.y), blob.size)
}
