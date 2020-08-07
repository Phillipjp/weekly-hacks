package blobs

import blobs.Main._
import org.scalatest._

class MainSpec extends FunSuite with Matchers {

  test("add given two numbers then returns sum") {
    add(2, 3) shouldBe 5
  }
}

