package cryptanalysis.vigenere

import cryptanalysis.caesar.CaesarCipherEncrypter
import cryptanalysis.Utils
import cryptanalysis.vigenere.VigenereCommon._

object VigenereCipherDecrypter {

  def decrypt(cipherText: String, keyString: String): String = {

    val normalizedPlainText = Utils.normalizeString(cipherText)
    val normalizedWithoutSpacesPlainText = Utils.normalizeStringWithoutSpaces(cipherText)

    val key = Utils.normalizeString(makeKey(normalizedWithoutSpacesPlainText, keyString))

    encryptText(normalizedPlainText, key, 0, makeVigenereDecrypterTable())
  }

  private [vigenere] def makeVigenereDecrypterTable(): Map[Char, Map[Char, Char]] = {
    val alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    alphabet.zip((0 to 25).map(shift => new CaesarCipherEncrypter(shift).encrypt(alphabet).zip(alphabet).toMap)).toMap
  }

}
