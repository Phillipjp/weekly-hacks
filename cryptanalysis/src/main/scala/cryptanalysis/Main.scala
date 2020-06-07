package cryptanalysis

import scala.io.{BufferedSource, Source}

object Main {

  def main(args: Array[String]): Unit = {

    val cypher = new CaesarCypherEncrypter(2)
    val plainText = "the quick brown fox jumps over the lazy dog"
    val cypherText = cypher.encrypt(plainText)
    val source  = Source.fromResource("bee-movie-script.txt")
    val sampleText = source.mkString
    source.close()

    val decrypter = new CaesarCypherDecrypter(sampleText)
    val result = decrypter.decryptCypherText(cypherText)

    println(plainText.toUpperCase)
    println(result)

  }
}
