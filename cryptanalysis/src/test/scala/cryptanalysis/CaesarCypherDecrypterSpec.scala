package cryptanalysis

import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers

class CaesarCypherDecrypterSpec extends AnyFlatSpecLike with Matchers{

  it should "order the characters in the sample text from most frequent to least frequent" in {
    // Given
    val sampleText = "DDDD CCC AA BB"
    val decrypter = new CaesarCypherDecrypter(sampleText)
    val expected = Seq(
      'D', 'C',  'A',  'B',  'E',  'F',  'G',  'H',  'I',
      'J', 'K',  'L',  'M',  'N',  'O',  'P',  'Q',  'R',
      'S', 'T',  'U',  'V',  'W',  'X',  'Y',  'Z')

    // When
    val actual = decrypter.getLetterFrequencies(sampleText)


    // Then
    actual shouldBe expected


  }

}
