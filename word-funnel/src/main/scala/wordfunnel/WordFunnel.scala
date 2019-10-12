package wordfunnel

import scala.annotation.tailrec

object WordFunnel {

  def funnel(words: Seq[String], dictionary: Set[String], funnelingFunc: (String, Set[String]) => Seq[String]): Int = {

    @tailrec
    def getFunnel(words: Seq[String], funnelLength: Int, dictionary: Set[String], funnelingFunc: (String, Set[String]) => Seq[String]): Int = {

      val funneledWords = words.flatMap(word => funnelingFunc(word, dictionary))

      if(funneledWords.isEmpty)
        funnelLength
      else
        getFunnel(funneledWords, funnelLength+1, dictionary, funnelingFunc)

    }
    getFunnel(words, 1, dictionary, funnelingFunc)

  }


}
