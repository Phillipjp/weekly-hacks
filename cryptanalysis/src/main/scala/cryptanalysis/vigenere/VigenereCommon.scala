package cryptanalysis.vigenere

import scala.annotation.tailrec

object VigenereCommon {


  private [vigenere] def makeKey(normalizedWithoutSpacesPlainText: String, keyString: String): String = {
    val s = (normalizedWithoutSpacesPlainText.length / keyString.length) + 1
    (keyString * s).take(normalizedWithoutSpacesPlainText.length)
  }

  @tailrec
  final def encryptText(text: String, key: String, i: Int, vigenereTable: Map[Char, Map[Char, Char]]): String = {
    if(key.isEmpty)
      text
    else
    {
      val currentChar = text.charAt(i)
      if(currentChar == ' ') {
        encryptText(text, key, i + 1, vigenereTable)
      } else{
        encryptText(text.substring(0, i) + vigenereTable(key.head)(currentChar).toString + text.substring(i+1),
          key.tail, i+1, vigenereTable)
      }
    }
  }

}
