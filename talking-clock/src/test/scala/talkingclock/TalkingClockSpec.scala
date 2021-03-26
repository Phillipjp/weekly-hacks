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

  it should "give an am time when the hour is less than 12" in {
    // Given, When, Then
    TalkingClock.timeToSentence(TimeString("02:00")) shouldBe "It's two am"
  }

  it should "give a pm time when the hour is greater than 11" in {
    // Given, When, Then
    TalkingClock.timeToSentence(TimeString("12:00")) shouldBe "It's twelve pm"
  }

  it should "correctly return the time as a sentence" in {
    // Given, When, Then
    TalkingClock.timeToSentence(TimeString("09:45")) shouldBe "It's nine forty five am"
  }




}
