package blobs

import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers

class GameSpec extends AnyFlatSpecLike with Matchers{

  behavior of "removeSmallestBlobs"

  it should "remove the smallest blobs" in{

    // Given
    val blobs = Seq(Blob2D(Coords2D(0,2),1), Blob2D(Coords2D(2,1),2), Blob2D(Coords2D(3,2),5), Blob2D(Coords2D(1,2),1))

    // When
    val actual = Game.removeSmallestBlobs(blobs)

    // Then
    actual shouldBe Seq(Blob2D(Coords2D(2,1),2), Blob2D(Coords2D(3,2),5))
  }

  behavior of "getSmallestBlobs"

  it should "get the smallest blobs" in {
    // Given
    val blobs = Seq(Blob2D(Coords2D(0,2),1), Blob2D(Coords2D(2,1),2), Blob2D(Coords2D(3,2),5), Blob2D(Coords2D(1,2),1))

    // When
    val actual = Game.getSmallestBlobs(blobs)

    // Then
    actual shouldBe Seq(Blob2D(Coords2D(0,2),1), Blob2D(Coords2D(1,2),1))

  }

  behavior of "moveBlobs"

  it should "it should move blobs to the next closest blob" in {
    // Given
    val blobs = Seq(Blob2D(Coords2D(4,4),1), Blob2D(Coords2D(4,3),2), Blob2D(Coords2D(1,1),3), Blob2D(Coords2D(1,2),4), Blob2D(Coords2D(2,1),2))
    val movableBlobs = Seq( Blob2D(Coords2D(4,3),2), Blob2D(Coords2D(1,1),3), Blob2D(Coords2D(1,2),4), Blob2D(Coords2D(2,1),2))

    // When
    val actual = Game.moveBlobs(blobs, movableBlobs)

    // Then
    actual shouldBe Seq(Blob2D(Coords2D(4,4),2), Blob2D(Coords2D(2,1),3), Blob2D(Coords2D(1,1),4), Blob2D(Coords2D(3,2),2))

  }

  behavior of "mergeBlobs"

  it should "it should merge blobs that have the same x and y values" in {
    // Given
    val blobs = Seq(Blob2D(Coords2D(4,4),1), Blob2D(Coords2D(4,4),2), Blob2D(Coords2D(1,1),3))

    // When
    val actual = Game.mergeBlobs(blobs)

    // Then
    actual should contain theSameElementsAs  Seq(Blob2D(Coords2D(4,4),3), Blob2D(Coords2D(1,1),3))

  }

  behavior of "turn"

  it should "it should play a complete turn of the game of blobs" in {
    // Given
    val blobs = Seq(Blob2D(Coords2D(4,4),1), Blob2D(Coords2D(4,3),2), Blob2D(Coords2D(1,1),3), Blob2D(Coords2D(1,2),4), Blob2D(Coords2D(2,1),2))

    // When
    val actual = Game.turn(blobs)

    // Then
    actual should contain theSameElementsAs  Seq(Blob2D(Coords2D(4,4),3), Blob2D(Coords2D(2,1),3), Blob2D(Coords2D(1,1),4), Blob2D(Coords2D(3,2),2))
  }

  behavior of "playBlobs"

  it should "it should play a complete  game of blobs" in {
    // Given
    val blobs = Seq(Blob2D(Coords2D(4,4),1), Blob2D(Coords2D(4,3),2), Blob2D(Coords2D(1,1),3), Blob2D(Coords2D(1,2),4), Blob2D(Coords2D(2,1),2))

    // When
    val actual = Game.playBlobs(blobs)

    // Then
    actual shouldBe Blob2D(Coords2D(3,2),12)
  }

}
