package blobs

trait BlobWrapper{
  val blobs: Seq[Blob]

  def size: Int = blobs.size

  def head: Blob = blobs.head

}

case class Blob2DWrapper(blobs: Seq[Blob2D]) extends BlobWrapper
case class Blob3DWrapper(blobs: Seq[Blob3D]) extends BlobWrapper


