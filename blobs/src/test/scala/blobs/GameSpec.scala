package blobs

import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers

class GameSpec extends AnyFlatSpecLike with Matchers{

  behavior of "turn"

  it should "it should play a complete turn of the game of blobs" in {
    // Given
    val blobs = Blob2DWrapper(Seq(Blob2D(Coord2D(4,4),1), Blob2D(Coord2D(4,3),2), Blob2D(Coord2D(1,1),3), Blob2D(Coord2D(1,2),4), Blob2D(Coord2D(2,1),2)))

    // When
    val actual = new Game(Coord2DWrapper(Seq(Coord2D(0,0))), blobs).turn(blobs)

    // Then
    actual.blobs should contain theSameElementsAs  Seq(Blob2D(Coord2D(4,4),3), Blob2D(Coord2D(2,1),3), Blob2D(Coord2D(1,1),4), Blob2D(Coord2D(3,2),2))
  }

  behavior of "playBlobs"

  it should "it should play a complete  game of blobs" in {
    // Given
    val blobs = Blob2DWrapper(Seq(Blob2D(Coord2D(4,4),1), Blob2D(Coord2D(4,3),2), Blob2D(Coord2D(1,1),3), Blob2D(Coord2D(1,2),4), Blob2D(Coord2D(2,1),2)))
    val coords = Coord2DWrapper(PlayArea2D.makePlayAreaCoords(Coord2D(0,0), Coord2D(4,4)))
    // When
    val actual = new Game(coords, blobs).playBlobs()

    // Then
    actual shouldBe Blob2D(Coord2D(3,2),12)
  }

}
