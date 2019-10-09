package wordfunnel

object WordFunnel {

  def funnel(word: String, funnelLength: Int, dictionary: Seq[String]): Int = {

    if(word.length == 2)
      funnelLength
    else{
      val funneledWords = Stream.continually(word).take(word.length).zipWithIndex
        .map{case (copy, index) =>
          copy.substring(0, index) + copy.substring(index + 1, copy.length)
        }
        .filter( funneledWord => dictionary.contains(funneledWord))

      if(funneledWords.isEmpty)
          funnelLength
      else
        funneledWords.map(funneledWord => funnel(funneledWord, funnelLength+1, dictionary)).max
    }

  }

}
