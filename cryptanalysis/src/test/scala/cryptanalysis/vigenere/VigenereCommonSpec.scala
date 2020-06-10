package cryptanalysis.vigenere

import cryptanalysis.Utils
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers

class VigenereCommonSpec extends AnyFlatSpecLike with Matchers{

  it should "make the encryption key from the key string" in {
    // Given
    val normalizedPlainText = Utils.normalizeStringWithoutSpaces("defghijk")

    // When
    val actual = VigenereCommon.makeKey(normalizedPlainText, "abc")

    // Then
    actual shouldBe "abcabcab"
  }

  it should "correctly encrypt text" in {
    // Given
    val expected = "XZB AC"

    // When
    val actual = VigenereCommon.encryptText("ABC DE", "XYZXY", 0, VigenereCipherEncrypter.makeVigenereTable())

    // Then
    actual shouldBe expected
  }

}
