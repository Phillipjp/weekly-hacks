package blobs

import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers

class GridSpec extends AnyFlatSpecLike with Matchers {

  it should "make a square 3x3 grid starting at (0, 0)" in {
    // Given
    val expected = List((0, 0), (1, 0), (2, 0),
                        (0, 1), (1, 1), (2, 1),
                        (0, 2), (1, 2), (2, 2))
    // When
    val actual = Grid.makeGridCoords(0, 0, 2, 2)
    // Then
    actual shouldBe expected
  }

  it should "make a rectangular 2x3 grid starting at (0, 0)" in {
    // Given
    val expected = List((0, 0), (1, 0),
                        (0, 1), (1, 1),
                        (0, 2), (1, 2))
    // When
    val actual = Grid.makeGridCoords(0, 0,   1, 2)
    // Then
    actual shouldBe expected
  }

  it should "make a square 3x3 grid starting at (-1, -1)" in {
    // Given
    val expected = List((-1, -1), (0, -1), (1, -1),
                        (-1, 0), (0, 0), (1, 0),
                        (-1, 1), (0, 1), (1, 1))
    // When
    val actual = Grid.makeGridCoords(-1, -1, 1, 1)
    // Then
    actual shouldBe expected
  }

}
