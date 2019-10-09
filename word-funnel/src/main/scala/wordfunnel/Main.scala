package wordfunnel

object Main {

  def main(args: Array[String]): Unit = {
    val dict = DictionaryReader.readInDictionary("/Users/perks1/Dev/my-stuff/weekly-hack/word-funnel/src/main/resources/enable1.txt")
//    val maxFunnelLength = WordFunnel.funnel("princesses", 1, dict)
//    println(maxFunnelLength)

    val biggestFunnel = dict.filter(word => word.length > 8).map(word => (word, WordFunnel.funnel(word, 1, dict))).maxBy{ case(_, funnelLength) => funnelLength}
    println(biggestFunnel)
  }
}
