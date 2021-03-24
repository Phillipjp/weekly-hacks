package talkingclock

import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers

class TalkingClockSpec extends AnyFlatSpecLike with Matchers {

  it should "get the am time identifier for times less than 12" in {
    // Given
    val hours = (0 to 11)
    val expected = Seq.fill(12)(Option("am"))

    // When
    val actual = hours.map(TalkingClock.getTimeIdentifier)

    // Then
    actual shouldBe expected
  }

  it should "get the pm time identifier for times greater than 11 and less than 24" in {
    // Given
    val hours = (12 to 23)
    val expected = Seq.fill(12)(Option("pm"))

    // When
    val actual = hours.map(TalkingClock.getTimeIdentifier)

    // Then
    actual shouldBe expected
  }

  it should "return none if the hour value is not a valid hour" in {
    // Given
    val hours = Seq(-1, 24)
    val expected = Seq(None, None)

    // When
    val actual = hours.map(TalkingClock.getTimeIdentifier)

    // Then
    actual shouldBe expected
  }


}
