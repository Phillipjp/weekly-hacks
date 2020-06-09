package cryptanalysis


import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers

class CaesarCipherEncrypterSpec extends AnyFlatSpecLike with Matchers{

  it should "create a caesar mapping with a shift of 2" in {
    // Given
    val caesarCypher = new CaesarCipherEncrypter(2)
    val expected = Map(
      'A' -> 'C', 'B' -> 'D', 'C' -> 'E', 'D' -> 'F', 'E' -> 'G', 'F' -> 'H', 'G' -> 'I', 'H' -> 'J', 'I' -> 'K',
      'J' -> 'L', 'K' -> 'M', 'L' -> 'N', 'M' -> 'O', 'N' -> 'P', 'O' -> 'Q', 'P' -> 'R', 'Q' -> 'S', 'R' -> 'T',
      'S' -> 'U', 'T' -> 'V', 'U' -> 'W', 'V' -> 'X', 'W' -> 'Y', 'X' -> 'Z', 'Y' -> 'A', 'Z' -> 'B', ' ' -> ' '
    )
      // When
      val actual = caesarCypher.getCaesarMapping()
      // Then
      expected shouldBe actual
  }

  it should "correctly encrypt a message with a positive shift" in {
    // Given
    val plainText = "ABC DEF"
    val caesarCypher = new CaesarCipherEncrypter(2)
    val expected = "CDE FGH"
    // When
    val actual = caesarCypher.encrypt(plainText)
    // Then
    actual shouldBe expected
  }

  it should "correctly encrypt a message with a negative shift" in {
    // Given
    val plainText = "ABC DEF"
    val caesarCypher = new CaesarCipherEncrypter(-2)
    val expected = "YZA BCD"
    // When
    val actual = caesarCypher.encrypt(plainText)
    // Then
    actual shouldBe expected
  }

}
