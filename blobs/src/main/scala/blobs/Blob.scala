package blobs

trait Blob[B <: Blob[B, C], C <: Coord] {
  val coord: C
  val size: Int

  def findClosestBlob(blobs: Seq[B]): B

  def moveTowardsBlob(blob: B): B

}
