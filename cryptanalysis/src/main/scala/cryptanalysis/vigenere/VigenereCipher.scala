package cryptanalysis.vigenere

import cryptanalysis.Cipher
import cryptanalysis.caesar.CaesarCipher
import cryptanalysis.language.Language

import scala.annotation.tailrec

object VigenereCipher extends Cipher[String]{

  override def encrypt(plainText: String, key: String, language: Language): String = {
    val normalizedPlainText = language.normalizeString(plainText)
    val normalizedWithoutSpacesPlainText = language.normalizeStringWithoutSpaces(plainText)

    val encryptionKey = language.normalizeString(makeKey(normalizedWithoutSpacesPlainText, key))

    encryptText(normalizedPlainText, encryptionKey, 0, makeVigenereTable(language))

  }

  private [vigenere] def makeVigenereTable(language: Language): Map[Char, Map[Char, Char]] = {
    language.alphabet.zip((0 to 25).map(shift => language.alphabet.zip(CaesarCipher.encrypt(language.alphabet, shift, language)).toMap)).toMap
  }

  override def decrypt(cipherText: String, key: String, language: Language): String = {

    val normalizedPlainText = language.normalizeString(cipherText)
    val normalizedWithoutSpacesPlainText = language.normalizeStringWithoutSpaces(cipherText)

    val encryptionKey = language.normalizeString(makeKey(normalizedWithoutSpacesPlainText, key))

    encryptText(normalizedPlainText, encryptionKey, 0, makeVigenereDecrypterTable(language))
  }

  private [vigenere] def makeVigenereDecrypterTable(language: Language): Map[Char, Map[Char, Char]] = {
    language.alphabet.zip((0 to 25).map(shift => CaesarCipher.encrypt(language.alphabet, shift, language).zip(language.alphabet).toMap)).toMap
  }

  private [vigenere] def makeKey(normalizedWithoutSpacesPlainText: String, keyString: String): String = {
    val s = (normalizedWithoutSpacesPlainText.length / keyString.length) + 1
    (keyString * s).take(normalizedWithoutSpacesPlainText.length)
  }

  @tailrec
  private [vigenere] final def encryptText(text: String, encryptionKey: String, i: Int, vigenereTable: Map[Char, Map[Char, Char]]): String = {
    if(encryptionKey.isEmpty)
      text
    else
    {
      val currentChar = text.charAt(i)
      if(currentChar == ' ') {
        encryptText(text, encryptionKey, i + 1, vigenereTable)
      } else{
        encryptText(text.substring(0, i) + vigenereTable(encryptionKey.head)(currentChar).toString + text.substring(i+1),
          encryptionKey.tail, i+1, vigenereTable)
      }
    }
  }

}
