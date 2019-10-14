package wordfunnel

import scala.annotation.tailrec

object FunnelingFunctions {

  def getFunneledWords(depth: Int)(word: String, dictionary: Set[String]): Seq[String] = {
    funnelWords(Seq(word), Seq(), depth, 0).distinct
      .filter( funneledWord => filterFunneledWord(dictionary, funneledWord))
  }

  @tailrec
  private def funnelWords(words: Seq[String], funneledWords: Seq[String], times: Int, i: Int): Seq[String] = {
    if(i == times || words.forall(w => w.length == 2))
      funneledWords
    else{
      val newWords = words.map(copy => Stream.continually(copy).take(copy.length).zipWithIndex)
        .flatMap(s => s.map{case (copy, index) => removeCharAtIndex(copy, index)})
      val newFunneledWords = (funneledWords ++ newWords).distinct
      funnelWords(newWords.distinct, newFunneledWords, times, i + 1)
    }
  }

  private def removeCharAtIndex(copy: String, index: Int) = {
    copy.substring(0, index) + copy.substring(index + 1, copy.length)
  }

  private def filterFunneledWord(dictionary: Set[String], funneledWord: String) = {
    dictionary.contains(funneledWord) && funneledWord.length > 1
  }


}
