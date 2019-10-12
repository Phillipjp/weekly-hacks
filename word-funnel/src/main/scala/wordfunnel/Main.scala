package wordfunnel
import wordfunnel.FunnelingFunctions._
import wordfunnel.WordFunnel.funnel

object Main {

  def main(args: Array[String]): Unit = {
    val dict = DictionaryReader.readInDictionary("/Users/phillipperks/Documents/repos/weekly-hacks/word-funnel/src/main/resources/enable1.txt")
    val maxFunnelLength = funnel(Seq("gnash"), dict, getFunneledWords)
    println(maxFunnelLength)

    val biggestFunnel = dict.filter(word => word.length > 10).map(word => (word, WordFunnel.funnel(Seq(word), dict, getFunneledWords))).maxBy{ case(_, funnelLength) => funnelLength}
    println(biggestFunnel)
    //(complecting,10)
  }
}
