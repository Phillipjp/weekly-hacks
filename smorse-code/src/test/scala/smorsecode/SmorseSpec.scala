package smorsecode

import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers

class SmorseSpec extends AnyFlatSpecLike with Matchers{

  it should "convert a string into smorse code" in {
    // Given, When
    val actual = Smorse.smorse("sos")

    // Then
    actual shouldBe "...---..."
  }

}
