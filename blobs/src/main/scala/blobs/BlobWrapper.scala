package blobs

trait BlobWrapper[B <: Blob[C], C <: Coord]{
  val blobs: Seq[B]

  def size: Int = blobs.size

  def head: B = blobs.head

}
