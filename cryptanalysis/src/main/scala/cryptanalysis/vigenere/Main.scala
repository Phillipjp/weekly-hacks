package cryptanalysis.vigenere

import cryptanalysis.language.English

object Main {

  def main(args: Array[String]): Unit = {

    val plainText = "the quick brown fox jumps over the lazy dog"
    val cipherText = VigenereCipher.encrypt(plainText, "recs", English)

//    println(s"PLAIN TEXT:  ${plainText.toUpperCase}")
    println(s"CIPHER TEXT: $cipherText")

    println("============================================================")
    val top5 = VigenereBreaker.break(cipherText, English)
    top5.zipWithIndex.foreach(x => println(s"BREAK ATTEMPT ${x._2+1}: ${x._1}"))
    println("============================================================")
    println(s"DECRYPT: ${VigenereCipher.decrypt(cipherText, "recs", English)}")

  }
}
