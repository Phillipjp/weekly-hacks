package blobs

trait Blob[C <: Coord] {
  val coord: C
  val size: Int

  def findClosestBlob(blobs: Seq[Blob[C]]): Blob[C]

  def moveTowardsBlob(blob: Blob[C]): Blob[C]

}
