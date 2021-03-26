package talkingclock

import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers

class MinuteToWordMapSpec extends AnyFlatSpecLike with Matchers {


  it should "get number 10 to 19 as words" in {
    // Given
    val minutesAsNumbers = (10 to 19).map(_.toString)
    val expected = List("ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen", "seventeen", "eighteen", "nineteen")
    // When
    val actual = minutesAsNumbers.map(MinuteToWordMap.get(_).get)
    // Then
    actual shouldBe expected
  }

  it should "get an empty string for 00" in {
    MinuteToWordMap.get("00").get shouldBe ""
  }

  it should "get a given minutes as a number as a word" in {
    // Given
    val minutesAsNumbers = Seq("05", "25", "35", "45", "55")
    val expected = Seq("oh five", "twenty five", "thirty five", "forty five", "fifty five")
    // When
    val actual = minutesAsNumbers.map(MinuteToWordMap.get(_).get)
    // Then
    actual shouldBe expected
  }

  it should "get the minute without a leading space when the minute is a multiple of 10" in {
    val minutesAsNumbers = Seq("10", "20", "30", "40", "50")
    val expected = Seq("ten", "twenty", "thirty", "forty", "fifty")
    // When
    val actual = minutesAsNumbers.map(MinuteToWordMap.get(_).get)
    // Then
    actual shouldBe expected
  }

  it should "get None for an invalid minute" in {
    MinuteToWordMap.get("65") shouldBe None
  }



}
