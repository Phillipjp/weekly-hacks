package cryptanalysis.caesar

import cryptanalysis.Cipher
import cryptanalysis.language.Language

object CaesarCipher extends Cipher[Int]{

  override def encrypt(plainText: String, shift: Int, language: Language): String = {
    val normalizedPlainText = language.normalizeString(plainText)
    val caesarMapping = getCaesarMapping(shift, language.alphabet)
    val encryptedText = normalizedPlainText.map(letter => caesarMapping(letter))
    encryptedText
  }

  override def decrypt(cypherText: String, shift: Int, language: Language): String = {
    encrypt(cypherText, language.alphabet.length - shift, language)
  }

  private[caesar] def getCaesarMapping(shift: Int, alphabet: String): Map[Char, Char] = {

    val standardisedShift = if (shift < 0) {
      alphabet.length + shift
    }
    else {
      shift
    }

    val firstHalf = alphabet.substring(0, alphabet.length - standardisedShift).map { letter => (letter + standardisedShift).toChar }
    val secondHalf = alphabet.take(standardisedShift)

    val encoded = firstHalf + secondHalf

    alphabet.zip(encoded).toMap + (' ' -> ' ')

  }

}
