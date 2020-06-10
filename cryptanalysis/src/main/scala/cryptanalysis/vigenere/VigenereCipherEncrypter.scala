package cryptanalysis.vigenere

import cryptanalysis.Utils
import cryptanalysis.caesar.CaesarCipherEncrypter
import VigenereCommon._

import scala.annotation.tailrec

object VigenereCipherEncrypter {

  private val vigenereTable = makeVigenereTable()

  def encrypt(plainText: String, keyString: String): String = {
    val normalizedPlainText = Utils.normalizeString(plainText)
    val normalizedWithoutSpacesPlainText = Utils.normalizeStringWithoutSpaces(plainText)

    val key = Utils.normalizeString(makeKey(normalizedWithoutSpacesPlainText, keyString))

    encryptText(normalizedPlainText, key, 0, vigenereTable)

  }

  private [vigenere] def makeVigenereTable(): Map[Char, Map[Char, Char]] = {
    val alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    alphabet.zip((0 to 25).map(shift => alphabet.zip(new CaesarCipherEncrypter(shift).encrypt(alphabet)).toMap)).toMap
  }

}
