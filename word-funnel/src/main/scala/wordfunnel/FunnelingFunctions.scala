package wordfunnel

object FunnelingFunctions {

  def getFunneledWords(word: String, dictionary: Set[String]): Seq[String] = {
    generateFunneledWords(word)
      .filter( funneledWord => filterFunneledWord(dictionary, funneledWord))
  }

  def getFunneledWords2(word: String, dictionary: Set[String]): Seq[String] = {
//    val oneLetterFunneledWords: Seq[String] = generateFunneledWords(word)
//
//    val twoLetterFunnelWords: Seq[String] = oneLetterFunneledWords.map(copy => Stream.continually(copy).take(copy.length).zipWithIndex)
//      .flatMap(s => s.map{case (copy, index) => removeCharAtIndex(copy, index)})
//
//    val funnelWords = oneLetterFunneledWords ++ twoLetterFunnelWords

    val funneledWords = funnelWords(Seq(word), Seq(), 5, 0).toSet
    funneledWords.toSeq.filter( funneledWord => filterFunneledWord(dictionary, funneledWord))

  }

  private def generateFunneledWords(word: String) = {
    Stream.continually(word).take(word.length).zipWithIndex
      .map { case (copy, index) => removeCharAtIndex(copy, index) }
  }

  def funnelWords(words: Seq[String], funneledWords: Seq[String], times: Int, i: Int): Seq[String] = {
    if(i == times || words.forall(w => w.length == 2))
      funneledWords
    else{
      val newWords = words.map(copy => Stream.continually(copy).take(copy.length).zipWithIndex)
        .flatMap(s => s.map{case (copy, index) => removeCharAtIndex(copy, index)})
      val newFunneledWords = (funneledWords ++ newWords).distinct
      funnelWords(newWords, newFunneledWords, times, i + 1)
    }
  }


  private def removeCharAtIndex(copy: String, index: Int) = {
    copy.substring(0, index) + copy.substring(index + 1, copy.length)
  }

  private def filterFunneledWord(dictionary: Set[String], funneledWord: String) = {
    dictionary.contains(funneledWord) && funneledWord.length > 1
  }

  def main(args: Array[String]): Unit = {
    val r = funnelWords(Seq("gnash"), Seq(), 4, 0)
    println(r)
  }

}
