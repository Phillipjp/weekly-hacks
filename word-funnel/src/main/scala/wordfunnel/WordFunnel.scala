package wordfunnel

object WordFunnel {

  def funnel(word: String, funnelLength: Int, dictionary: Seq[String], funnelingFunc: (String, Seq[String]) => Seq[String]): Int = {

    if(word.length == 2)
      funnelLength
    else{
      val funneledWords = funnelingFunc(word, dictionary)

      if(funneledWords.isEmpty)
          funnelLength
      else
        funneledWords.map(funneledWord => funnel(funneledWord, funnelLength+1, dictionary, funnelingFunc)).max
    }
  }

}
