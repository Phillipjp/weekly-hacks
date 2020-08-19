package blobs

import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers

class BlobsSpec extends AnyFlatSpecLike with Matchers{

    behavior of "removeSmallestBlobs"

    it should "remove the smallest blobs" in{

      // Given
      val blobs = Seq(Blob2D(Coord2D(0,2),1), Blob2D(Coord2D(2,1),2), Blob2D(Coord2D(3,2),5), Blob2D(Coord2D(1,2),1))

      // When
      val actual = Blobs.removeSmallestBlobs(blobs)

      // Then
      actual shouldBe Seq(Blob2D(Coord2D(2,1),2), Blob2D(Coord2D(3,2),5))
    }

    behavior of "getSmallestBlobs"

    it should "get the smallest blobs" in {
      // Given
      val blobs = Seq(Blob2D(Coord2D(0,2),1), Blob2D(Coord2D(2,1),2), Blob2D(Coord2D(3,2),5), Blob2D(Coord2D(1,2),1))

      // When
      val actual = Blobs.getSmallestBlobs(blobs)

      // Then
      actual shouldBe Seq(Blob2D(Coord2D(0,2),1), Blob2D(Coord2D(1,2),1))

    }

    behavior of "moveBlobs"

    it should "it should move 2D blobs to the next closest blob" in {
      // Given
      val blobs = Seq(Blob2D(Coord2D(4,4),1), Blob2D(Coord2D(4,3),2), Blob2D(Coord2D(1,1),3), Blob2D(Coord2D(1,2),4), Blob2D(Coord2D(2,1),2))
      val movableBlobs = Seq( Blob2D(Coord2D(4,3),2), Blob2D(Coord2D(1,1),3), Blob2D(Coord2D(1,2),4), Blob2D(Coord2D(2,1),2))

      // When
      val actual = Blobs.moveBlobs(blobs, movableBlobs, Blob2D.findClosestBlob, Blob2D.moveTowardsBlob)

      // Then
      actual shouldBe Seq(Blob2D(Coord2D(4,4),2), Blob2D(Coord2D(2,1),3), Blob2D(Coord2D(1,1),4), Blob2D(Coord2D(3,2),2))

    }

    behavior of "mergeBlobs"

    it should "it should merge blobs that have the same coords" in {
      // Given
      val movedBlobs = Seq(Blob2D(Coord2D(4,4),2), Blob2D(Coord2D(1,1),3))
      val smallBlobs = Seq(Blob2D(Coord2D(4,4),1))

      // When
      val actual = Blobs.mergeBlobs(movedBlobs, smallBlobs)

      // Then
      actual should contain theSameElementsAs  Seq(Blob2D(Coord2D(4,4),3), Blob2D(Coord2D(1,1),3))

    }

}
