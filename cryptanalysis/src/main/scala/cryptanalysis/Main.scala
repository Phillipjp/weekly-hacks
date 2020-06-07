package cryptanalysis

import scala.io.{BufferedSource, Source}
import scala.util.Random

object Main {

  def main(args: Array[String]): Unit = {

    val cypher = new CaesarCypherEncrypter(2)
    val plainText = "the quick brown fox jumps over the lazy dog"
    val cypherText = cypher.encrypt(plainText)

    val sampleText = "louofumnhtwroxrkzqojthaepdyecbvsegi"

    val decrypter = new CaesarCypherDecrypter(sampleText)
    val result = decrypter.decryptCypherText(cypherText)

    println(plainText.toUpperCase)
    println(result)

  }
}
