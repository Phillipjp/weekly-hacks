package talkingclock

import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers

class TimeStringSpec extends AnyFlatSpecLike with Matchers {

  it should "throw an exception if the string is less than 5 characters long" in {
    val thrown = intercept[Exception] {
      TimeString("1234")
    }

    thrown.getMessage shouldBe "requirement failed"
  }

  it should "throw an exception if the string is greater than 5 characters long" in {
    val thrown = intercept[Exception] {
      TimeString("123456")
    }

    thrown.getMessage shouldBe "requirement failed"
  }

  it should "throw an exception if the string is missing a ':'" in {
    val thrown = intercept[Exception] {
      TimeString("12345")
    }

    thrown.getMessage shouldBe "requirement failed"
  }

  it should "throw an exception if the string is missing a ':' at index 2" in {
    val thrown = intercept[Exception] {
      TimeString("1:234")
    }

    thrown.getMessage shouldBe "requirement failed"
  }

  it should "throw an exception if any of the characters aren't digits apart form the colon" in {
    val thrown = intercept[Exception] {
      TimeString("a2:34")
    }

    thrown.getMessage shouldBe "requirement failed"
  }

  it should "throw an exception if the hours are greater than 23" in {
    val thrown = intercept[Exception] {
      TimeString("24:05")
    }

    thrown.getMessage shouldBe "requirement failed"
  }

  it should "throw an exception if the hours are less than 00" in {
    val thrown = intercept[Exception] {
      TimeString("-1:05")
    }

    thrown.getMessage shouldBe "requirement failed"
  }

  it should "throw an exception if the minutes are greater than 59" in {
    val thrown = intercept[Exception] {
      TimeString("00:60")
    }

    thrown.getMessage shouldBe "requirement failed"
  }

  it should "throw an exception if the minutes are less than 00" in {
    val thrown = intercept[Exception] {
      TimeString("00:-1")
    }

    thrown.getMessage shouldBe "requirement failed"
  }

}
