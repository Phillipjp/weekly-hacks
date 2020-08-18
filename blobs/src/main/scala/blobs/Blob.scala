package blobs

trait Blob[B <: Blob[B, C], C <: Coord] {
  val coord: C
  val size: Int

  def findClosestBlob(blobs: Seq[Blob[B,C]]): Blob[B, C]

  def moveTowardsBlob(blob: Blob[B,C]): Blob[B,C]

}
