package cryptanalysis

import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers

class CaesarCypherDecrypterSpec extends AnyFlatSpecLike with Matchers{

  it should "order the characters in the sample text from most frequent to least frequent" in {
    // Given
    val sampleText = "DDDD CCC AA BB"
    val decrypter = new CaesarCypherDecrypter(sampleText)
    val expected = Seq('D', 'C',  'A',  'B')

    // When
    val actual = decrypter.getLetterFrequencies(sampleText)

    // Then
    actual shouldBe expected
  }

  it should "order the bigrams in the sample text from most common to least common" in {
    // Given
    val decrypter = new CaesarCypherDecrypter("")
    val expected = List("AB", "BA", "EF", "GH", "CA", "FG")

    // When
    val actual = decrypter.getNgramFrequencies("CABAB EFGH", 2)

    // Then
    actual shouldBe expected
  }

  it should "order the trigrams in the sample text from most common to least common" in {
    // Given
    val decrypter = new CaesarCypherDecrypter("")
    val expected = List("ABC", "XAB", "YAB", "CYA", "ZAB", "BCY")

    // When
    val actual = decrypter.getNgramFrequencies("XABCYABC ZABC", 3)

    // Then
    actual shouldBe expected
  }

  it should "correctly decrypt a message" in {
    // Given
    val cypher = new CaesarCypherEncrypter(2)
    val cypherText = cypher.encrypt("hello there")
    val decrypter = new CaesarCypherDecrypter("heerlelhot")
    val expected = "HELLO THERE"

    // When
    val actual = decrypter.decryptCypherText(cypherText)

    // Then
    actual shouldBe expected
  }

  it should "mark an unknown decrypted char as '?'" in {
    // Given
    val cypher = new CaesarCypherEncrypter(2)
    val cypherText = cypher.encrypt("hello there")
    val decrypter = new CaesarCypherDecrypter("heelelho")
    val expected = "HELLO ?HE?E"

    // When
    val actual = decrypter.decryptCypherText(cypherText)

    // Then
    actual shouldBe expected
  }


}
