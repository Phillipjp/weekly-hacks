package cryptanalysis.vigenere

import cryptanalysis.language.English
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers

class VigenereBreakerSpec extends AnyFlatSpecLike with Matchers{

  it should "return 5 different attempts at breaking the cipher" in {
    // Given
    val cipherText = VigenereCipher.encrypt("the quick brown fox jumps over the lazy dog", "bcd", English)

    // When
    val actual = VigenereBreaker.break(cipherText, English)

    // Then
    actual.size shouldBe 5
  }

  it should "get all possible substrings greater than length 2" in {
    // Given
    val cipherText = "A BCDE"
    val expected = Seq("ABC", "BCD", "CDE", "ABCD", "BCDE", "ABCDE")

    // When
    val actual = VigenereBreaker.getAllPossibleConsecutiveSubstrings(cipherText)

    // Then
    actual shouldBe expected
  }


  it should "get the distance between a given substring in a string" in {
    // Given

    val string = "PORWJSXFGXTHJAQWNFGXQ"
    val substring = "FGX"

    // When
    val actual = VigenereBreaker.getDistanceBetweenSubstrings(string, substring)

    // Then
    actual shouldBe 10
  }

  it should "return -1 when substring is not in string" in {
    // Given

    val string = "PORWJSXFGXTHJAQWNFGXQ"
    val substring = "ABC"

    // When
    val actual = VigenereBreaker.getDistanceBetweenSubstrings(string, substring)

    // Then
    actual shouldBe -1
  }

  it should "get all factors of a number" in {
    // When
    val actual = VigenereBreaker.factorize(18)

    // Then
    actual shouldBe Seq(3,6,9,18)
  }

  it should "get all the mode values in a list" in {
    // Given
    val list = Seq(1,2,2,3,4,4)

    // When
    val actual = VigenereBreaker.getModes(list)

    // Then
    actual shouldBe Set(2,4)
  }

  it should "get the modal value from a list" in {
    // Given
    val list = Seq(1,2,2,3,4)

    // When
    val actual = VigenereBreaker.getMode(list)

    // Then
    actual shouldBe 2
  }

  it should "zip 3 strings together" in {
    // Given
    val strings = Seq("147", "258", "36")

    // When
    val actual = VigenereBreaker.zipStrings(strings, "")

    // Then
    actual shouldBe "12345678"
  }

}
