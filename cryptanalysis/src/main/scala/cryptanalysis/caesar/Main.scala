package cryptanalysis.caesar

import cryptanalysis.language.English

object Main {

  def main(args: Array[String]): Unit = {

    val plainText = "the quick brown fox jumps over the lazy dog"
    val cipherText = CaesarCipher.encrypt(plainText, 2, English)

    println(s"PLAIN TEXT:  ${plainText.toUpperCase}")
    println(s"CIPHER TEXT: $cipherText")

    println("============================================================")
    val top5 = CaesarBreaker.break(cipherText, English)
    top5.zipWithIndex.foreach(x => println(s"BREAK ATTEMPT ${x._2+1}: ${x._1}"))
    println("============================================================")
    println(s"DECRYPT: ${CaesarCipher.decrypt(cipherText, 2, English)}")

  }
}
