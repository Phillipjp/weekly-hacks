package cryptanalysis

object Main {

  def main(args: Array[String]): Unit = {

    val cypher = new CaesarCipherEncrypter(2)
    val plainText = "the quick brown fox jumps over the lazy dog"
    val cypherText = cypher.encrypt(plainText)

    println(s"PLAIN TEXT:  ${plainText.toUpperCase}")
    println(s"CIPHER TEXT: $cypherText")

    println("========================================================")
    val top5 = CaesarCipherDecrypter.decrypt(cypherText)
    top5.zipWithIndex.foreach(x => println(s"DECIPHER ${x._2+1}: ${x._1}"))

  }
}
