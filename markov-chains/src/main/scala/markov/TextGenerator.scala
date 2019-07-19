package markov

import dto.{BiGram, WordCount}
import markov.MarkovChain._

import scala.util.Random

object TextGenerator {

  val markovChain = makeMarkovChain("src/main/resources/archer.txt")
  val length = 3000

  private def generateTextSequence(text: Seq[String]): Seq[String] = {
    if(text.size > length && text.last.last == '.')
      text
    else{
      val key = text.drop(text.size - 2)
      val newText: Seq[String] = text :+ getNewWord( markovChain(key))
      generateTextSequence(newText)
    }

  }

  private def getNewWord(wordCount: WordCount): String = {
    val limit = wordCount.values.sum
    val rand = Random.nextInt(limit)
    val words = wordCount.keys.toList.flatMap{ key =>
      List.fill(wordCount(key))(key)
    }
    words(rand)
  }


  def generateText(): String = {

    generateTextSequence(getStart).mkString(" ")

  }

  def getStart: BiGram = {
    val key = pickRandomKey
    if(key.head.charAt(0).isUpper)
      key
    else
      getStart
  }

  private def pickRandomKey = {
    val keys = markovChain.keys.toSeq
    val rand = Random.nextInt(keys.size)
    keys(rand)
  }

  def main(args: Array[String]): Unit = {
    println(generateText())
  }

}
