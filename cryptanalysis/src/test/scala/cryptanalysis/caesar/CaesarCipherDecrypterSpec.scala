package cryptanalysis.caesar

import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers

class CaesarCipherDecrypterSpec extends AnyFlatSpecLike with Matchers{


  it should "correctly decrypt the cipher when the shift is known" in {
    // Given
    val shift = 2
    val plainText = "hello there"
    val cipherText = new CaesarCipherEncrypter(shift).encrypt(plainText)

    // When
    val actual = CaesarCipherDecrypter.decrypt(cipherText, shift)

    // Then
    actual shouldBe plainText.toUpperCase
  }

  it should "calculate the frequency of each character in a string" in {
    // Given
    val initalFrequecies = Map('A' -> 0, 'B' -> 0)
    val sampleText = "AABAB"
    val expected = Map('A' -> 3, 'B' -> 2)

    // When
    val actual = CaesarCipherDecrypter.getFrequencies(sampleText, initalFrequecies)

    // Then
    actual shouldBe expected
  }

  it should "calculate the chi squared result between two lists" in {
    // Given
    val observedFrequencies = Seq(5,8,5)
    val expectedFrequencies = Seq(4.5,10.2,6.7)

    val expected = 0.9614086430592136

    // When
    val actual = CaesarCipherDecrypter.chiSquare(observedFrequencies, expectedFrequencies)

    // Then
    actual shouldBe expected
  }

  it should "throw exception when observed length doesn't equal expected length" in {
    // Given
    val observedFrequencies = Seq(5,8,5)
    val expectedFrequencies = Seq(4.5,10.2)

    val expected = s"length of observed frequencies (${observedFrequencies.length}) does not match the length of expected frequencies (${expectedFrequencies.length})"

    // When
    val actual = intercept[Exception] {
      CaesarCipherDecrypter.chiSquare(observedFrequencies, expectedFrequencies)
    }

    // Then
    actual.getMessage shouldBe expected
  }

  it should "return 5 different attempts at breaking the cipher" in {
    // Given
    val cipher = new CaesarCipherEncrypter(2)
    val plainText = "the quick brown fox jumps over the lazy dog"
    val cipherText = cipher.encrypt(plainText)

    // When
    val actual = CaesarCipherDecrypter.breakCipher(cipherText)

    // Then
    actual.size shouldBe 5


  }

}
