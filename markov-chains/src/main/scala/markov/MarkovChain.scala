package markov




import dto.{BiGram, WordCount}

import scala.io.Source


object MarkovChain {

  def textTrimmer(text: String): Boolean =
    text != "" && text != "-"

  private def readFile(path: String): Seq[String] = {
    Source.fromFile(path).getLines.toSeq.flatMap( line => line.split(" ").filter(textTrimmer))
  }

  private def generateBiGramMap(groupedText: Seq[Seq[String]], biGramMap: Map[BiGram, WordCount]): Map[BiGram, WordCount] = {

    if(groupedText.isEmpty)
      biGramMap
    else{
      val head: Seq[String] = groupedText.head
      val biGramKey = Seq(head(0), head(1))
      val wordCountKey: String = head(2)

      if(biGramMap.contains(biGramKey)){
        if(biGramMap(biGramKey).contains(wordCountKey)){
          val updatedValue = biGramMap(biGramKey)(wordCountKey) + 1
          val updatedWordCount = biGramMap(biGramKey) + (wordCountKey -> updatedValue)
          val updatedBiGramMap = biGramMap + (biGramKey -> updatedWordCount)
          generateBiGramMap(groupedText.drop(1), updatedBiGramMap)
        }
        else {
          val updatedWordCount = biGramMap(biGramKey) + (wordCountKey -> 1)
          val updatedBiGramMap = biGramMap + (biGramKey -> updatedWordCount)
          generateBiGramMap(groupedText.drop(1), updatedBiGramMap)
        }
      }
      else{
        val wordCount = Map(wordCountKey -> 1)
        val updatedBiGramMap = biGramMap + (biGramKey -> wordCount)
        generateBiGramMap(groupedText.drop(1), updatedBiGramMap)
      }
    }



  }

  def makeMarkovChain(path: String): Map[BiGram, WordCount] = {
    val text = readFile(path)
    val groupedText = text.sliding(3).toSeq
    generateBiGramMap(groupedText, Map())


  }

  def main(args: Array[String]): Unit = {
    makeMarkovChain("/Users/perks1/Dev/my-stuff/weekly-hack/markov-chains/src/main/resources/archer.txt").foreach(println)
  }

}
