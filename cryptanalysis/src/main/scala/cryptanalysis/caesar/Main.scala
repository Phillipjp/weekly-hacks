package cryptanalysis.caesar

object Main {

  def main(args: Array[String]): Unit = {

    val cipher = new CaesarCipherEncrypter(2)
    val plainText = "the quick brown fox jumps over the lazy dog"
    val cipherText = cipher.encrypt(plainText)

    println(s"PLAIN TEXT:  ${plainText.toUpperCase}")
    println(s"CIPHER TEXT: $cipherText")

    println("============================================================")
    val top5 = CaesarCipherDecrypter.breakCipher(cipherText)
    top5.zipWithIndex.foreach(x => println(s"BREAK ATTEMPT ${x._2+1}: ${x._1}"))
    println("============================================================")
    println(s"DECRYPT: ${CaesarCipherDecrypter.decrypt(cipherText, 2)}")

  }
}
