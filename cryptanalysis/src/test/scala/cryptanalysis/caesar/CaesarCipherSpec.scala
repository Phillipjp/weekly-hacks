package cryptanalysis.caesar

import cryptanalysis.language.English
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers

class CaesarCipherSpec extends AnyFlatSpecLike with Matchers{

  it should "create a caesar mapping with a shift of 2" in {
    // Given
    val expected = Map(
      'A' -> 'C', 'B' -> 'D', 'C' -> 'E', 'D' -> 'F', 'E' -> 'G', 'F' -> 'H', 'G' -> 'I', 'H' -> 'J', 'I' -> 'K',
      'J' -> 'L', 'K' -> 'M', 'L' -> 'N', 'M' -> 'O', 'N' -> 'P', 'O' -> 'Q', 'P' -> 'R', 'Q' -> 'S', 'R' -> 'T',
      'S' -> 'U', 'T' -> 'V', 'U' -> 'W', 'V' -> 'X', 'W' -> 'Y', 'X' -> 'Z', 'Y' -> 'A', 'Z' -> 'B', ' ' -> ' '
    )
      // When
      val actual = CaesarCipher.getCaesarMapping(2, English.alphabet)
      // Then
      expected shouldBe actual
  }

  it should "correctly encrypt a message with a positive shift" in {
    // Given
    val plainText = "ABC DEF"
    val expected = "CDE FGH"
    // When
    val actual = CaesarCipher.encrypt(plainText, 2, English)
    // Then
    actual shouldBe expected
  }

  it should "correctly encrypt a message with a negative shift" in {
    // Given
    val plainText = "ABC DEF"
    val expected = "YZA BCD"
    // When
    val actual = CaesarCipher.encrypt(plainText, -2, English)
    // Then
    actual shouldBe expected
  }

  it should "correctly decrypt the cipher when the shift is known" in {
    // Given
    val shift = 2
    val plainText = "hello there"
    val cipherText = CaesarCipher.encrypt(plainText, shift, English)

    // When
    val actual = CaesarCipher.decrypt(cipherText, shift, English)

    // Then
    actual shouldBe plainText.toUpperCase
  }

}
