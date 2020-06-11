package cryptanalysis.vigenere

import cryptanalysis.language.English
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers

class VigenereCipherSpec extends AnyFlatSpecLike with Matchers{

  it should "make the Vigenere table" in {

    val expected = Map(
        ('E',Map('E' -> 'I', 'X' -> 'B', 'N' -> 'R', 'T' -> 'X', 'Y' -> 'C', 'J' -> 'N', 'U' -> 'Y', 'F' -> 'J', 'A' -> 'E', 'M' -> 'Q', 'I' -> 'M', 'G' -> 'K', 'V' -> 'Z', 'Q' -> 'U', 'L' -> 'P', 'B' -> 'F', 'P' -> 'T', 'C' -> 'G', 'H' -> 'L', 'W' -> 'A', 'K' -> 'O', 'R' -> 'V', 'O' -> 'S', 'D' -> 'H', 'Z' -> 'D', 'S' -> 'W')),
        ('X',Map('E' -> 'B', 'X' -> 'U', 'N' -> 'K', 'T' -> 'Q', 'Y' -> 'V', 'J' -> 'G', 'U' -> 'R', 'F' -> 'C', 'A' -> 'X', 'M' -> 'J', 'I' -> 'F', 'G' -> 'D', 'V' -> 'S', 'Q' -> 'N', 'L' -> 'I', 'B' -> 'Y', 'P' -> 'M', 'C' -> 'Z', 'H' -> 'E', 'W' -> 'T', 'K' -> 'H', 'R' -> 'O', 'O' -> 'L', 'D' -> 'A', 'Z' -> 'W', 'S' -> 'P')),
        ('N',Map('E' -> 'R', 'X' -> 'K', 'N' -> 'A', 'T' -> 'G', 'Y' -> 'L', 'J' -> 'W', 'U' -> 'H', 'F' -> 'S', 'A' -> 'N', 'M' -> 'Z', 'I' -> 'V', 'G' -> 'T', 'V' -> 'I', 'Q' -> 'D', 'L' -> 'Y', 'B' -> 'O', 'P' -> 'C', 'C' -> 'P', 'H' -> 'U', 'W' -> 'J', 'K' -> 'X', 'R' -> 'E', 'O' -> 'B', 'D' -> 'Q', 'Z' -> 'M', 'S' -> 'F')),
        ('T',Map('E' -> 'X', 'X' -> 'Q', 'N' -> 'G', 'T' -> 'M', 'Y' -> 'R', 'J' -> 'C', 'U' -> 'N', 'F' -> 'Y', 'A' -> 'T', 'M' -> 'F', 'I' -> 'B', 'G' -> 'Z', 'V' -> 'O', 'Q' -> 'J', 'L' -> 'E', 'B' -> 'U', 'P' -> 'I', 'C' -> 'V', 'H' -> 'A', 'W' -> 'P', 'K' -> 'D', 'R' -> 'K', 'O' -> 'H', 'D' -> 'W', 'Z' -> 'S', 'S' -> 'L')),
        ('Y',Map('E' -> 'C', 'X' -> 'V', 'N' -> 'L', 'T' -> 'R', 'Y' -> 'W', 'J' -> 'H', 'U' -> 'S', 'F' -> 'D', 'A' -> 'Y', 'M' -> 'K', 'I' -> 'G', 'G' -> 'E', 'V' -> 'T', 'Q' -> 'O', 'L' -> 'J', 'B' -> 'Z', 'P' -> 'N', 'C' -> 'A', 'H' -> 'F', 'W' -> 'U', 'K' -> 'I', 'R' -> 'P', 'O' -> 'M', 'D' -> 'B', 'Z' -> 'X', 'S' -> 'Q')),
        ('J',Map('E' -> 'N', 'X' -> 'G', 'N' -> 'W', 'T' -> 'C', 'Y' -> 'H', 'J' -> 'S', 'U' -> 'D', 'F' -> 'O', 'A' -> 'J', 'M' -> 'V', 'I' -> 'R', 'G' -> 'P', 'V' -> 'E', 'Q' -> 'Z', 'L' -> 'U', 'B' -> 'K', 'P' -> 'Y', 'C' -> 'L', 'H' -> 'Q', 'W' -> 'F', 'K' -> 'T', 'R' -> 'A', 'O' -> 'X', 'D' -> 'M', 'Z' -> 'I', 'S' -> 'B')),
        ('U',Map('E' -> 'Y', 'X' -> 'R', 'N' -> 'H', 'T' -> 'N', 'Y' -> 'S', 'J' -> 'D', 'U' -> 'O', 'F' -> 'Z', 'A' -> 'U', 'M' -> 'G', 'I' -> 'C', 'G' -> 'A', 'V' -> 'P', 'Q' -> 'K', 'L' -> 'F', 'B' -> 'V', 'P' -> 'J', 'C' -> 'W', 'H' -> 'B', 'W' -> 'Q', 'K' -> 'E', 'R' -> 'L', 'O' -> 'I', 'D' -> 'X', 'Z' -> 'T', 'S' -> 'M')),
        ('F',Map('E' -> 'J', 'X' -> 'C', 'N' -> 'S', 'T' -> 'Y', 'Y' -> 'D', 'J' -> 'O', 'U' -> 'Z', 'F' -> 'K', 'A' -> 'F', 'M' -> 'R', 'I' -> 'N', 'G' -> 'L', 'V' -> 'A', 'Q' -> 'V', 'L' -> 'Q', 'B' -> 'G', 'P' -> 'U', 'C' -> 'H', 'H' -> 'M', 'W' -> 'B', 'K' -> 'P', 'R' -> 'W', 'O' -> 'T', 'D' -> 'I', 'Z' -> 'E', 'S' -> 'X')),
        ('A',Map('E' -> 'E', 'X' -> 'X', 'N' -> 'N', 'T' -> 'T', 'Y' -> 'Y', 'J' -> 'J', 'U' -> 'U', 'F' -> 'F', 'A' -> 'A', 'M' -> 'M', 'I' -> 'I', 'G' -> 'G', 'V' -> 'V', 'Q' -> 'Q', 'L' -> 'L', 'B' -> 'B', 'P' -> 'P', 'C' -> 'C', 'H' -> 'H', 'W' -> 'W', 'K' -> 'K', 'R' -> 'R', 'O' -> 'O', 'D' -> 'D', 'Z' -> 'Z', 'S' -> 'S')),
        ('M',Map('E' -> 'Q', 'X' -> 'J', 'N' -> 'Z', 'T' -> 'F', 'Y' -> 'K', 'J' -> 'V', 'U' -> 'G', 'F' -> 'R', 'A' -> 'M', 'M' -> 'Y', 'I' -> 'U', 'G' -> 'S', 'V' -> 'H', 'Q' -> 'C', 'L' -> 'X', 'B' -> 'N', 'P' -> 'B', 'C' -> 'O', 'H' -> 'T', 'W' -> 'I', 'K' -> 'W', 'R' -> 'D', 'O' -> 'A', 'D' -> 'P', 'Z' -> 'L', 'S' -> 'E')),
        ('I',Map('E' -> 'M', 'X' -> 'F', 'N' -> 'V', 'T' -> 'B', 'Y' -> 'G', 'J' -> 'R', 'U' -> 'C', 'F' -> 'N', 'A' -> 'I', 'M' -> 'U', 'I' -> 'Q', 'G' -> 'O', 'V' -> 'D', 'Q' -> 'Y', 'L' -> 'T', 'B' -> 'J', 'P' -> 'X', 'C' -> 'K', 'H' -> 'P', 'W' -> 'E', 'K' -> 'S', 'R' -> 'Z', 'O' -> 'W', 'D' -> 'L', 'Z' -> 'H', 'S' -> 'A')),
        ('G',Map('E' -> 'K', 'X' -> 'D', 'N' -> 'T', 'T' -> 'Z', 'Y' -> 'E', 'J' -> 'P', 'U' -> 'A', 'F' -> 'L', 'A' -> 'G', 'M' -> 'S', 'I' -> 'O', 'G' -> 'M', 'V' -> 'B', 'Q' -> 'W', 'L' -> 'R', 'B' -> 'H', 'P' -> 'V', 'C' -> 'I', 'H' -> 'N', 'W' -> 'C', 'K' -> 'Q', 'R' -> 'X', 'O' -> 'U', 'D' -> 'J', 'Z' -> 'F', 'S' -> 'Y')),
        ('V',Map('E' -> 'Z', 'X' -> 'S', 'N' -> 'I', 'T' -> 'O', 'Y' -> 'T', 'J' -> 'E', 'U' -> 'P', 'F' -> 'A', 'A' -> 'V', 'M' -> 'H', 'I' -> 'D', 'G' -> 'B', 'V' -> 'Q', 'Q' -> 'L', 'L' -> 'G', 'B' -> 'W', 'P' -> 'K', 'C' -> 'X', 'H' -> 'C', 'W' -> 'R', 'K' -> 'F', 'R' -> 'M', 'O' -> 'J', 'D' -> 'Y', 'Z' -> 'U', 'S' -> 'N')),
        ('Q',Map('E' -> 'U', 'X' -> 'N', 'N' -> 'D', 'T' -> 'J', 'Y' -> 'O', 'J' -> 'Z', 'U' -> 'K', 'F' -> 'V', 'A' -> 'Q', 'M' -> 'C', 'I' -> 'Y', 'G' -> 'W', 'V' -> 'L', 'Q' -> 'G', 'L' -> 'B', 'B' -> 'R', 'P' -> 'F', 'C' -> 'S', 'H' -> 'X', 'W' -> 'M', 'K' -> 'A', 'R' -> 'H', 'O' -> 'E', 'D' -> 'T', 'Z' -> 'P', 'S' -> 'I')),
        ('L',Map('E' -> 'P', 'X' -> 'I', 'N' -> 'Y', 'T' -> 'E', 'Y' -> 'J', 'J' -> 'U', 'U' -> 'F', 'F' -> 'Q', 'A' -> 'L', 'M' -> 'X', 'I' -> 'T', 'G' -> 'R', 'V' -> 'G', 'Q' -> 'B', 'L' -> 'W', 'B' -> 'M', 'P' -> 'A', 'C' -> 'N', 'H' -> 'S', 'W' -> 'H', 'K' -> 'V', 'R' -> 'C', 'O' -> 'Z', 'D' -> 'O', 'Z' -> 'K', 'S' -> 'D')),
        ('B',Map('E' -> 'F', 'X' -> 'Y', 'N' -> 'O', 'T' -> 'U', 'Y' -> 'Z', 'J' -> 'K', 'U' -> 'V', 'F' -> 'G', 'A' -> 'B', 'M' -> 'N', 'I' -> 'J', 'G' -> 'H', 'V' -> 'W', 'Q' -> 'R', 'L' -> 'M', 'B' -> 'C', 'P' -> 'Q', 'C' -> 'D', 'H' -> 'I', 'W' -> 'X', 'K' -> 'L', 'R' -> 'S', 'O' -> 'P', 'D' -> 'E', 'Z' -> 'A', 'S' -> 'T')),
        ('P',Map('E' -> 'T', 'X' -> 'M', 'N' -> 'C', 'T' -> 'I', 'Y' -> 'N', 'J' -> 'Y', 'U' -> 'J', 'F' -> 'U', 'A' -> 'P', 'M' -> 'B', 'I' -> 'X', 'G' -> 'V', 'V' -> 'K', 'Q' -> 'F', 'L' -> 'A', 'B' -> 'Q', 'P' -> 'E', 'C' -> 'R', 'H' -> 'W', 'W' -> 'L', 'K' -> 'Z', 'R' -> 'G', 'O' -> 'D', 'D' -> 'S', 'Z' -> 'O', 'S' -> 'H')),
        ('C',Map('E' -> 'G', 'X' -> 'Z', 'N' -> 'P', 'T' -> 'V', 'Y' -> 'A', 'J' -> 'L', 'U' -> 'W', 'F' -> 'H', 'A' -> 'C', 'M' -> 'O', 'I' -> 'K', 'G' -> 'I', 'V' -> 'X', 'Q' -> 'S', 'L' -> 'N', 'B' -> 'D', 'P' -> 'R', 'C' -> 'E', 'H' -> 'J', 'W' -> 'Y', 'K' -> 'M', 'R' -> 'T', 'O' -> 'Q', 'D' -> 'F', 'Z' -> 'B', 'S' -> 'U')),
        ('H',Map('E' -> 'L', 'X' -> 'E', 'N' -> 'U', 'T' -> 'A', 'Y' -> 'F', 'J' -> 'Q', 'U' -> 'B', 'F' -> 'M', 'A' -> 'H', 'M' -> 'T', 'I' -> 'P', 'G' -> 'N', 'V' -> 'C', 'Q' -> 'X', 'L' -> 'S', 'B' -> 'I', 'P' -> 'W', 'C' -> 'J', 'H' -> 'O', 'W' -> 'D', 'K' -> 'R', 'R' -> 'Y', 'O' -> 'V', 'D' -> 'K', 'Z' -> 'G', 'S' -> 'Z')),
        ('W',Map('E' -> 'A', 'X' -> 'T', 'N' -> 'J', 'T' -> 'P', 'Y' -> 'U', 'J' -> 'F', 'U' -> 'Q', 'F' -> 'B', 'A' -> 'W', 'M' -> 'I', 'I' -> 'E', 'G' -> 'C', 'V' -> 'R', 'Q' -> 'M', 'L' -> 'H', 'B' -> 'X', 'P' -> 'L', 'C' -> 'Y', 'H' -> 'D', 'W' -> 'S', 'K' -> 'G', 'R' -> 'N', 'O' -> 'K', 'D' -> 'Z', 'Z' -> 'V', 'S' -> 'O')),
        ('K',Map('E' -> 'O', 'X' -> 'H', 'N' -> 'X', 'T' -> 'D', 'Y' -> 'I', 'J' -> 'T', 'U' -> 'E', 'F' -> 'P', 'A' -> 'K', 'M' -> 'W', 'I' -> 'S', 'G' -> 'Q', 'V' -> 'F', 'Q' -> 'A', 'L' -> 'V', 'B' -> 'L', 'P' -> 'Z', 'C' -> 'M', 'H' -> 'R', 'W' -> 'G', 'K' -> 'U', 'R' -> 'B', 'O' -> 'Y', 'D' -> 'N', 'Z' -> 'J', 'S' -> 'C')),
        ('R',Map('E' -> 'V', 'X' -> 'O', 'N' -> 'E', 'T' -> 'K', 'Y' -> 'P', 'J' -> 'A', 'U' -> 'L', 'F' -> 'W', 'A' -> 'R', 'M' -> 'D', 'I' -> 'Z', 'G' -> 'X', 'V' -> 'M', 'Q' -> 'H', 'L' -> 'C', 'B' -> 'S', 'P' -> 'G', 'C' -> 'T', 'H' -> 'Y', 'W' -> 'N', 'K' -> 'B', 'R' -> 'I', 'O' -> 'F', 'D' -> 'U', 'Z' -> 'Q', 'S' -> 'J')),
        ('O',Map('E' -> 'S', 'X' -> 'L', 'N' -> 'B', 'T' -> 'H', 'Y' -> 'M', 'J' -> 'X', 'U' -> 'I', 'F' -> 'T', 'A' -> 'O', 'M' -> 'A', 'I' -> 'W', 'G' -> 'U', 'V' -> 'J', 'Q' -> 'E', 'L' -> 'Z', 'B' -> 'P', 'P' -> 'D', 'C' -> 'Q', 'H' -> 'V', 'W' -> 'K', 'K' -> 'Y', 'R' -> 'F', 'O' -> 'C', 'D' -> 'R', 'Z' -> 'N', 'S' -> 'G')),
        ('D',Map('E' -> 'H', 'X' -> 'A', 'N' -> 'Q', 'T' -> 'W', 'Y' -> 'B', 'J' -> 'M', 'U' -> 'X', 'F' -> 'I', 'A' -> 'D', 'M' -> 'P', 'I' -> 'L', 'G' -> 'J', 'V' -> 'Y', 'Q' -> 'T', 'L' -> 'O', 'B' -> 'E', 'P' -> 'S', 'C' -> 'F', 'H' -> 'K', 'W' -> 'Z', 'K' -> 'N', 'R' -> 'U', 'O' -> 'R', 'D' -> 'G', 'Z' -> 'C', 'S' -> 'V')),
        ('Z',Map('E' -> 'D', 'X' -> 'W', 'N' -> 'M', 'T' -> 'S', 'Y' -> 'X', 'J' -> 'I', 'U' -> 'T', 'F' -> 'E', 'A' -> 'Z', 'M' -> 'L', 'I' -> 'H', 'G' -> 'F', 'V' -> 'U', 'Q' -> 'P', 'L' -> 'K', 'B' -> 'A', 'P' -> 'O', 'C' -> 'B', 'H' -> 'G', 'W' -> 'V', 'K' -> 'J', 'R' -> 'Q', 'O' -> 'N', 'D' -> 'C', 'Z' -> 'Y', 'S' -> 'R')),
        ('S',Map('E' -> 'W', 'X' -> 'P', 'N' -> 'F', 'T' -> 'L', 'Y' -> 'Q', 'J' -> 'B', 'U' -> 'M', 'F' -> 'X', 'A' -> 'S', 'M' -> 'E', 'I' -> 'A', 'G' -> 'Y', 'V' -> 'N', 'Q' -> 'I', 'L' -> 'D', 'B' -> 'T', 'P' -> 'H', 'C' -> 'U', 'H' -> 'Z', 'W' -> 'O', 'K' -> 'C', 'R' -> 'J', 'O' -> 'G', 'D' -> 'V', 'Z' -> 'R', 'S' -> 'K'))
    )


    val actual = VigenereCipher.makeVigenereTable(English)

    actual shouldBe expected
    
  }

  it should "correctly encrypt text given only the plain text and key" in {
    // Given
    val expected = "XZB AC"

    // When
    val actual = VigenereCipher.encrypt("abc de", "xyz", English)

    // Then
    actual shouldBe expected
  }

  it should "make the encryption key from the key string" in {
    // Given
    val normalizedPlainText = English.normalizeStringWithoutSpaces("defghijk")

    // When
    val actual = VigenereCipher.makeKey(normalizedPlainText, "abc")

    // Then
    actual shouldBe "abcabcab"
  }

  it should "correctly encrypt text given the normalized plain text and encryption key" in {
    // Given
    val expected = "XZB AC"

    // When
    val actual = VigenereCipher.encryptText("ABC DE", "XYZXY", 0, VigenereCipher.makeVigenereTable(English))

    // Then
    actual shouldBe expected
  }

  it should "make the Vigenere table for decryption" in {
    val expected = Map(
      ('E',Map('E' -> 'A', 'X' -> 'T', 'N' -> 'J', 'T' -> 'P', 'Y' -> 'U', 'J' -> 'F', 'U' -> 'Q', 'F' -> 'B', 'A' -> 'W', 'M' -> 'I', 'I' -> 'E', 'G' -> 'C', 'V' -> 'R', 'Q' -> 'M', 'L' -> 'H', 'B' -> 'X', 'P' -> 'L', 'C' -> 'Y', 'H' -> 'D', 'W' -> 'S', 'K' -> 'G', 'R' -> 'N', 'O' -> 'K', 'D' -> 'Z', 'Z' -> 'V', 'S' -> 'O')),
      ('X',Map('E' -> 'H', 'X' -> 'A', 'N' -> 'Q', 'T' -> 'W', 'Y' -> 'B', 'J' -> 'M', 'U' -> 'X', 'F' -> 'I', 'A' -> 'D', 'M' -> 'P', 'I' -> 'L', 'G' -> 'J', 'V' -> 'Y', 'Q' -> 'T', 'L' -> 'O', 'B' -> 'E', 'P' -> 'S', 'C' -> 'F', 'H' -> 'K', 'W' -> 'Z', 'K' -> 'N', 'R' -> 'U', 'O' -> 'R', 'D' -> 'G', 'Z' -> 'C', 'S' -> 'V')),
      ('N',Map('E' -> 'R', 'X' -> 'K', 'N' -> 'A', 'T' -> 'G', 'Y' -> 'L', 'J' -> 'W', 'U' -> 'H', 'F' -> 'S', 'A' -> 'N', 'M' -> 'Z', 'I' -> 'V', 'G' -> 'T', 'V' -> 'I', 'Q' -> 'D', 'L' -> 'Y', 'B' -> 'O', 'P' -> 'C', 'C' -> 'P', 'H' -> 'U', 'W' -> 'J', 'K' -> 'X', 'R' -> 'E', 'O' -> 'B', 'D' -> 'Q', 'Z' -> 'M', 'S' -> 'F')),
      ('T',Map('E' -> 'L', 'X' -> 'E', 'N' -> 'U', 'T' -> 'A', 'Y' -> 'F', 'J' -> 'Q', 'U' -> 'B', 'F' -> 'M', 'A' -> 'H', 'M' -> 'T', 'I' -> 'P', 'G' -> 'N', 'V' -> 'C', 'Q' -> 'X', 'L' -> 'S', 'B' -> 'I', 'P' -> 'W', 'C' -> 'J', 'H' -> 'O', 'W' -> 'D', 'K' -> 'R', 'R' -> 'Y', 'O' -> 'V', 'D' -> 'K', 'Z' -> 'G', 'S' -> 'Z')),
      ('Y',Map('E' -> 'G', 'X' -> 'Z', 'N' -> 'P', 'T' -> 'V', 'Y' -> 'A', 'J' -> 'L', 'U' -> 'W', 'F' -> 'H', 'A' -> 'C', 'M' -> 'O', 'I' -> 'K', 'G' -> 'I', 'V' -> 'X', 'Q' -> 'S', 'L' -> 'N', 'B' -> 'D', 'P' -> 'R', 'C' -> 'E', 'H' -> 'J', 'W' -> 'Y', 'K' -> 'M', 'R' -> 'T', 'O' -> 'Q', 'D' -> 'F', 'Z' -> 'B', 'S' -> 'U')),
      ('J',Map('E' -> 'V', 'X' -> 'O', 'N' -> 'E', 'T' -> 'K', 'Y' -> 'P', 'J' -> 'A', 'U' -> 'L', 'F' -> 'W', 'A' -> 'R', 'M' -> 'D', 'I' -> 'Z', 'G' -> 'X', 'V' -> 'M', 'Q' -> 'H', 'L' -> 'C', 'B' -> 'S', 'P' -> 'G', 'C' -> 'T', 'H' -> 'Y', 'W' -> 'N', 'K' -> 'B', 'R' -> 'I', 'O' -> 'F', 'D' -> 'U', 'Z' -> 'Q', 'S' -> 'J')),
      ('U',Map('E' -> 'K', 'X' -> 'D', 'N' -> 'T', 'T' -> 'Z', 'Y' -> 'E', 'J' -> 'P', 'U' -> 'A', 'F' -> 'L', 'A' -> 'G', 'M' -> 'S', 'I' -> 'O', 'G' -> 'M', 'V' -> 'B', 'Q' -> 'W', 'L' -> 'R', 'B' -> 'H', 'P' -> 'V', 'C' -> 'I', 'H' -> 'N', 'W' -> 'C', 'K' -> 'Q', 'R' -> 'X', 'O' -> 'U', 'D' -> 'J', 'Z' -> 'F', 'S' -> 'Y')),
      ('F',Map('E' -> 'Z', 'X' -> 'S', 'N' -> 'I', 'T' -> 'O', 'Y' -> 'T', 'J' -> 'E', 'U' -> 'P', 'F' -> 'A', 'A' -> 'V', 'M' -> 'H', 'I' -> 'D', 'G' -> 'B', 'V' -> 'Q', 'Q' -> 'L', 'L' -> 'G', 'B' -> 'W', 'P' -> 'K', 'C' -> 'X', 'H' -> 'C', 'W' -> 'R', 'K' -> 'F', 'R' -> 'M', 'O' -> 'J', 'D' -> 'Y', 'Z' -> 'U', 'S' -> 'N')),
      ('A',Map('E' -> 'E', 'X' -> 'X', 'N' -> 'N', 'T' -> 'T', 'Y' -> 'Y', 'J' -> 'J', 'U' -> 'U', 'F' -> 'F', 'A' -> 'A', 'M' -> 'M', 'I' -> 'I', 'G' -> 'G', 'V' -> 'V', 'Q' -> 'Q', 'L' -> 'L', 'B' -> 'B', 'P' -> 'P', 'C' -> 'C', 'H' -> 'H', 'W' -> 'W', 'K' -> 'K', 'R' -> 'R', 'O' -> 'O', 'D' -> 'D', 'Z' -> 'Z', 'S' -> 'S')),
      ('M',Map('E' -> 'S', 'X' -> 'L', 'N' -> 'B', 'T' -> 'H', 'Y' -> 'M', 'J' -> 'X', 'U' -> 'I', 'F' -> 'T', 'A' -> 'O', 'M' -> 'A', 'I' -> 'W', 'G' -> 'U', 'V' -> 'J', 'Q' -> 'E', 'L' -> 'Z', 'B' -> 'P', 'P' -> 'D', 'C' -> 'Q', 'H' -> 'V', 'W' -> 'K', 'K' -> 'Y', 'R' -> 'F', 'O' -> 'C', 'D' -> 'R', 'Z' -> 'N', 'S' -> 'G')),
      ('I',Map('E' -> 'W', 'X' -> 'P', 'N' -> 'F', 'T' -> 'L', 'Y' -> 'Q', 'J' -> 'B', 'U' -> 'M', 'F' -> 'X', 'A' -> 'S', 'M' -> 'E', 'I' -> 'A', 'G' -> 'Y', 'V' -> 'N', 'Q' -> 'I', 'L' -> 'D', 'B' -> 'T', 'P' -> 'H', 'C' -> 'U', 'H' -> 'Z', 'W' -> 'O', 'K' -> 'C', 'R' -> 'J', 'O' -> 'G', 'D' -> 'V', 'Z' -> 'R', 'S' -> 'K')),
      ('G',Map('E' -> 'Y', 'X' -> 'R', 'N' -> 'H', 'T' -> 'N', 'Y' -> 'S', 'J' -> 'D', 'U' -> 'O', 'F' -> 'Z', 'A' -> 'U', 'M' -> 'G', 'I' -> 'C', 'G' -> 'A', 'V' -> 'P', 'Q' -> 'K', 'L' -> 'F', 'B' -> 'V', 'P' -> 'J', 'C' -> 'W', 'H' -> 'B', 'W' -> 'Q', 'K' -> 'E', 'R' -> 'L', 'O' -> 'I', 'D' -> 'X', 'Z' -> 'T', 'S' -> 'M')),
      ('V',Map('E' -> 'J', 'X' -> 'C', 'N' -> 'S', 'T' -> 'Y', 'Y' -> 'D', 'J' -> 'O', 'U' -> 'Z', 'F' -> 'K', 'A' -> 'F', 'M' -> 'R', 'I' -> 'N', 'G' -> 'L', 'V' -> 'A', 'Q' -> 'V', 'L' -> 'Q', 'B' -> 'G', 'P' -> 'U', 'C' -> 'H', 'H' -> 'M', 'W' -> 'B', 'K' -> 'P', 'R' -> 'W', 'O' -> 'T', 'D' -> 'I', 'Z' -> 'E', 'S' -> 'X')),
      ('Q',Map('E' -> 'O', 'X' -> 'H', 'N' -> 'X', 'T' -> 'D', 'Y' -> 'I', 'J' -> 'T', 'U' -> 'E', 'F' -> 'P', 'A' -> 'K', 'M' -> 'W', 'I' -> 'S', 'G' -> 'Q', 'V' -> 'F', 'Q' -> 'A', 'L' -> 'V', 'B' -> 'L', 'P' -> 'Z', 'C' -> 'M', 'H' -> 'R', 'W' -> 'G', 'K' -> 'U', 'R' -> 'B', 'O' -> 'Y', 'D' -> 'N', 'Z' -> 'J', 'S' -> 'C')),
      ('L',Map('E' -> 'T', 'X' -> 'M', 'N' -> 'C', 'T' -> 'I', 'Y' -> 'N', 'J' -> 'Y', 'U' -> 'J', 'F' -> 'U', 'A' -> 'P', 'M' -> 'B', 'I' -> 'X', 'G' -> 'V', 'V' -> 'K', 'Q' -> 'F', 'L' -> 'A', 'B' -> 'Q', 'P' -> 'E', 'C' -> 'R', 'H' -> 'W', 'W' -> 'L', 'K' -> 'Z', 'R' -> 'G', 'O' -> 'D', 'D' -> 'S', 'Z' -> 'O', 'S' -> 'H')),
      ('B',Map('E' -> 'D', 'X' -> 'W', 'N' -> 'M', 'T' -> 'S', 'Y' -> 'X', 'J' -> 'I', 'U' -> 'T', 'F' -> 'E', 'A' -> 'Z', 'M' -> 'L', 'I' -> 'H', 'G' -> 'F', 'V' -> 'U', 'Q' -> 'P', 'L' -> 'K', 'B' -> 'A', 'P' -> 'O', 'C' -> 'B', 'H' -> 'G', 'W' -> 'V', 'K' -> 'J', 'R' -> 'Q', 'O' -> 'N', 'D' -> 'C', 'Z' -> 'Y', 'S' -> 'R')),
      ('P',Map('E' -> 'P', 'X' -> 'I', 'N' -> 'Y', 'T' -> 'E', 'Y' -> 'J', 'J' -> 'U', 'U' -> 'F', 'F' -> 'Q', 'A' -> 'L', 'M' -> 'X', 'I' -> 'T', 'G' -> 'R', 'V' -> 'G', 'Q' -> 'B', 'L' -> 'W', 'B' -> 'M', 'P' -> 'A', 'C' -> 'N', 'H' -> 'S', 'W' -> 'H', 'K' -> 'V', 'R' -> 'C', 'O' -> 'Z', 'D' -> 'O', 'Z' -> 'K', 'S' -> 'D')),
      ('C',Map('E' -> 'C', 'X' -> 'V', 'N' -> 'L', 'T' -> 'R', 'Y' -> 'W', 'J' -> 'H', 'U' -> 'S', 'F' -> 'D', 'A' -> 'Y', 'M' -> 'K', 'I' -> 'G', 'G' -> 'E', 'V' -> 'T', 'Q' -> 'O', 'L' -> 'J', 'B' -> 'Z', 'P' -> 'N', 'C' -> 'A', 'H' -> 'F', 'W' -> 'U', 'K' -> 'I', 'R' -> 'P', 'O' -> 'M', 'D' -> 'B', 'Z' -> 'X', 'S' -> 'Q')),
      ('H',Map('E' -> 'X', 'X' -> 'Q', 'N' -> 'G', 'T' -> 'M', 'Y' -> 'R', 'J' -> 'C', 'U' -> 'N', 'F' -> 'Y', 'A' -> 'T', 'M' -> 'F', 'I' -> 'B', 'G' -> 'Z', 'V' -> 'O', 'Q' -> 'J', 'L' -> 'E', 'B' -> 'U', 'P' -> 'I', 'C' -> 'V', 'H' -> 'A', 'W' -> 'P', 'K' -> 'D', 'R' -> 'K', 'O' -> 'H', 'D' -> 'W', 'Z' -> 'S', 'S' -> 'L')),
      ('W',Map('E' -> 'I', 'X' -> 'B', 'N' -> 'R', 'T' -> 'X', 'Y' -> 'C', 'J' -> 'N', 'U' -> 'Y', 'F' -> 'J', 'A' -> 'E', 'M' -> 'Q', 'I' -> 'M', 'G' -> 'K', 'V' -> 'Z', 'Q' -> 'U', 'L' -> 'P', 'B' -> 'F', 'P' -> 'T', 'C' -> 'G', 'H' -> 'L', 'W' -> 'A', 'K' -> 'O', 'R' -> 'V', 'O' -> 'S', 'D' -> 'H', 'Z' -> 'D', 'S' -> 'W')),
      ('K',Map('E' -> 'U', 'X' -> 'N', 'N' -> 'D', 'T' -> 'J', 'Y' -> 'O', 'J' -> 'Z', 'U' -> 'K', 'F' -> 'V', 'A' -> 'Q', 'M' -> 'C', 'I' -> 'Y', 'G' -> 'W', 'V' -> 'L', 'Q' -> 'G', 'L' -> 'B', 'B' -> 'R', 'P' -> 'F', 'C' -> 'S', 'H' -> 'X', 'W' -> 'M', 'K' -> 'A', 'R' -> 'H', 'O' -> 'E', 'D' -> 'T', 'Z' -> 'P', 'S' -> 'I')),
      ('R',Map('E' -> 'N', 'X' -> 'G', 'N' -> 'W', 'T' -> 'C', 'Y' -> 'H', 'J' -> 'S', 'U' -> 'D', 'F' -> 'O', 'A' -> 'J', 'M' -> 'V', 'I' -> 'R', 'G' -> 'P', 'V' -> 'E', 'Q' -> 'Z', 'L' -> 'U', 'B' -> 'K', 'P' -> 'Y', 'C' -> 'L', 'H' -> 'Q', 'W' -> 'F', 'K' -> 'T', 'R' -> 'A', 'O' -> 'X', 'D' -> 'M', 'Z' -> 'I', 'S' -> 'B')),
      ('O',Map('E' -> 'Q', 'X' -> 'J', 'N' -> 'Z', 'T' -> 'F', 'Y' -> 'K', 'J' -> 'V', 'U' -> 'G', 'F' -> 'R', 'A' -> 'M', 'M' -> 'Y', 'I' -> 'U', 'G' -> 'S', 'V' -> 'H', 'Q' -> 'C', 'L' -> 'X', 'B' -> 'N', 'P' -> 'B', 'C' -> 'O', 'H' -> 'T', 'W' -> 'I', 'K' -> 'W', 'R' -> 'D', 'O' -> 'A', 'D' -> 'P', 'Z' -> 'L', 'S' -> 'E')),
      ('D',Map('E' -> 'B', 'X' -> 'U', 'N' -> 'K', 'T' -> 'Q', 'Y' -> 'V', 'J' -> 'G', 'U' -> 'R', 'F' -> 'C', 'A' -> 'X', 'M' -> 'J', 'I' -> 'F', 'G' -> 'D', 'V' -> 'S', 'Q' -> 'N', 'L' -> 'I', 'B' -> 'Y', 'P' -> 'M', 'C' -> 'Z', 'H' -> 'E', 'W' -> 'T', 'K' -> 'H', 'R' -> 'O', 'O' -> 'L', 'D' -> 'A', 'Z' -> 'W', 'S' -> 'P')),
      ('Z',Map('E' -> 'F', 'X' -> 'Y', 'N' -> 'O', 'T' -> 'U', 'Y' -> 'Z', 'J' -> 'K', 'U' -> 'V', 'F' -> 'G', 'A' -> 'B', 'M' -> 'N', 'I' -> 'J', 'G' -> 'H', 'V' -> 'W', 'Q' -> 'R', 'L' -> 'M', 'B' -> 'C', 'P' -> 'Q', 'C' -> 'D', 'H' -> 'I', 'W' -> 'X', 'K' -> 'L', 'R' -> 'S', 'O' -> 'P', 'D' -> 'E', 'Z' -> 'A', 'S' -> 'T')),
      ('S',Map('E' -> 'M', 'X' -> 'F', 'N' -> 'V', 'T' -> 'B', 'Y' -> 'G', 'J' -> 'R', 'U' -> 'C', 'F' -> 'N', 'A' -> 'I', 'M' -> 'U', 'I' -> 'Q', 'G' -> 'O', 'V' -> 'D', 'Q' -> 'Y', 'L' -> 'T', 'B' -> 'J', 'P' -> 'X', 'C' -> 'K', 'H' -> 'P', 'W' -> 'E', 'K' -> 'S', 'R' -> 'Z', 'O' -> 'W', 'D' -> 'L', 'Z' -> 'H', 'S' -> 'A'))
    )

    val actual = VigenereCipher.makeVigenereDecrypterTable(English)

    actual shouldBe expected

  }

  it should "correctly decrypt text given the plain text and key" in {
    // Given
    val cipherText = "XZB AC"
    val key = "xyz"

    val expected = "ABC DE"

    // When
    val actual = VigenereCipher.decrypt(cipherText, key, English)

    // Then
    actual shouldBe expected
  }

}
