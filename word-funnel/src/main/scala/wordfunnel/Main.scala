package wordfunnel
import wordfunnel.FunnelingFunctions._

object Main {

  def main(args: Array[String]): Unit = {
    val dict = DictionaryReader.readInDictionary("/Users/perks1/Dev/my-stuff/weekly-hack/word-funnel/src/main/resources/enable1.txt")
//    val maxFunnelLength = funnel(Seq("preformationists"), dict, getFunneledWords(2))
//    println(maxFunnelLength)
//
    /*Bonus 1*/
//    val biggestFunnel = dict.filter(word => word.length > 10).map(word => (word, WordFunnel.funnel(Seq(word), dict, getFunneledWords(1)))).maxBy{ case(_, funnelLength) => funnelLength}
//    println(biggestFunnel)
    //(complecting,10)

    /*Bonus 2*/
    val biggestFunnel2 = dict.filter(word => word.length > 12).map(word => (word, WordFunnel.funnel(Seq(word), dict, getFunneledWords(5)))).toSeq.sortBy{ case(_, funnelLength) => - funnelLength}.take(6)
    biggestFunnel2.foreach(println)
    //(contradictorinesses,12)
    //(nonrepresentationalisms,12)
    //(establishmentarianisms,12)
    //(preformationists,12)
    //(unrepresentativenesses,12)
    //(noncooperationists,12)
  }
}
