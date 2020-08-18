package blobs

import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers

class PlayArea2DSpec extends AnyFlatSpecLike with Matchers {

  it should "make a square 3x3 grid starting at (0, 0)" in {
    // Given
    val expected = List(Coord2D(0, 0), Coord2D(1, 0), Coord2D(2, 0),
                        Coord2D(0, 1), Coord2D(1, 1), Coord2D(2, 1),
                        Coord2D(0, 2), Coord2D(1, 2), Coord2D(2, 2))
    // When
    val actual = PlayArea2D.makePlayAreaCoords(Coord2D(0, 0), Coord2D(2, 2))
    // Then
    actual shouldBe expected
  }

  it should "make a rectangular 2x3 grid starting at (0, 0)" in {
    // Given
    val expected = List(Coord2D(0, 0), Coord2D(1, 0),
                        Coord2D(0, 1), Coord2D(1, 1),
                        Coord2D(0, 2), Coord2D(1, 2))
    // When
    val actual = PlayArea2D.makePlayAreaCoords(Coord2D(0, 0), Coord2D(1, 2))
    // Then
    actual shouldBe expected
  }

  it should "make a square 3x3 grid starting at (-1, -1)" in {
    // Given
    val expected = List(Coord2D(-1, -1),Coord2D(0, -1),Coord2D(1, -1),
                        Coord2D(-1, 0), Coord2D(0, 0), Coord2D(1, 0),
                        Coord2D(-1, 1), Coord2D(0, 1), Coord2D(1, 1))
    // When
    val actual = PlayArea2D.makePlayAreaCoords(Coord2D(-1, -1), Coord2D(1, 1))
    // Then
    actual shouldBe expected
  }

}
