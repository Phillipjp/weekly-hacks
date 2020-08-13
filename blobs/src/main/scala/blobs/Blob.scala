package blobs

trait Blob[B <: Blob[B, C], C <: Coords] {
  val coord: C
  val size: Int

  def findClosestBlob(blobs: Seq[B]): B

  def moveTowardsBlob(blob: B): B

}
