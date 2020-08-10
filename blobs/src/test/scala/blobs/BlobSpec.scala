package blobs

import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers

class BlobSpec extends AnyFlatSpecLike with Matchers {

  behavior of "distanceGrid"

  it should "create a distance grid where the value of each cell is the distance from the blob" in {

    // Given
    val size = 6

    val expected = Map((2,5) -> 2, (1,5) -> 2, (5,0) -> 3, (0,2) -> 3, (0,0) -> 3, (5,2) -> 2, (5,1) -> 2, (4,0) -> 3, (3,4) -> 1, (6,4) -> 3, (6,6) -> 3, (3,1) -> 2, (6,1) -> 3, (4,1) -> 2, (6,2) -> 3, (2,0) -> 3, (0,3) -> 3, (4,4) -> 1, (3,0) -> 3, (1,6) -> 3, (0,5) -> 3, (3,6) -> 3, (6,5) -> 3, (1,1) -> 2, (6,3) -> 3, (3,5) -> 2, (4,6) -> 3, (4,5) -> 2, (1,4) -> 2, (2,6) -> 3, (0,4) -> 3, (5,4) -> 2, (3,2) -> 1, (1,3) -> 2, (2,2) -> 1, (5,5) -> 2, (4,2) -> 1, (2,4) -> 1, (0,1) -> 3, (5,3) -> 2, (3,3) -> 0, (2,3) -> 1, (1,2) -> 2, (2,1) -> 2, (4,3) -> 1, (6,0) -> 3, (1,0) -> 3, (5,6) -> 3, (0,6) -> 3)

    // when
    val actual = Blob(3,3,1).distanceGrid(size)

//    println(g)
//    for(y <- size to 0 by -1){
//      for(x <- 0 to size ){
//        val value: Int = g((x,y))
//        print(s"$value ")
//      }
//      println()
//    }

    actual shouldBe expected
  }

  behavior of "findClosestBlob"

  it should "find the closest blob when the distance between blobs is different" in {
    // Given
    val blob = Blob(3,3,1)
    val a = Blob(3,5,1)
    val b = Blob(3,6,1)

    // When
    val actual = blob.findClosestBlob(Seq(a,b))

    // Then
    actual shouldBe a

  }

  it should "find the closest blob when the distance between blobs is equal" in {
    // Given
    val blob = Blob(3,3,1)
    val a = Blob(3,0,1)
    val b = Blob(4,0,1)

    // When
    val actual = blob.findClosestBlob(Seq(a,b))

    // Then
    actual shouldBe a

  }

  behavior of "moveTowardsBlob"

  it should "move the blob in a north direction given another blob directly above" in {
    Blob(3,3,1).moveTowardsBlob(Blob(3,5,1)) shouldBe Blob(3,4,1)
  }

  it should "move a blob south direction given another blob directly below" in {
    Blob(3,3,1).moveTowardsBlob(Blob(3,1,1)) shouldBe Blob(3,2,1)
  }

  it should "move a blob east given another blob directly right" in {
    Blob(3,3,1).moveTowardsBlob(Blob(5,3,1)) shouldBe Blob(4,3,1)
  }

  it should "move a blob west given another blob directly left" in {
    Blob(3,3,1).moveTowardsBlob(Blob(1,3,1)) shouldBe Blob(2,3,1)
  }

  it should "move the blob in a north east direction given another blob above and to the right" in {
    Blob(3,3,1).moveTowardsBlob(Blob(5,5,1)) shouldBe Blob(4,4,1)
  }

  it should "move the blob in a south east direction given another blob below and to the right" in {
    Blob(3,3,1).moveTowardsBlob(Blob(5,1,1)) shouldBe Blob(4,2,1)
  }

  it should "move the blob in a south west direction given another blob below and to the left" in {
    Blob(3,3,1).moveTowardsBlob(Blob(1,1,1)) shouldBe Blob(2,2,1)
  }

  it should "move the blob in a north west direction given another blob above and to the left" in {
    Blob(3,3,1).moveTowardsBlob(Blob(1,5,1)) shouldBe Blob(2,4,1)
  }

}
