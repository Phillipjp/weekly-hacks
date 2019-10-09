package wordfunnel

object FunnelingFunctions {

  def getFunneledWords(word: String, dictionary: Seq[String]): Seq[String] = {
    Stream.continually(word).take(word.length).zipWithIndex
      .map{case (copy, index) => removeCharAtIndex(copy, index)}
      .filter( funneledWord => dictionary.contains(funneledWord))
  }

  def getFunneledWords2(word: String, dictionary: Seq[String]): Seq[String] = {
    val oneLetterFunneledWords: Seq[String] = Stream.continually(word).take(word.length).zipWithIndex
      .map{case (copy, index) => removeCharAtIndex(copy, index)}

    val twoLetterFunnelWords: Seq[String] = oneLetterFunneledWords.map(copy => Stream.continually(copy).take(copy.length).zipWithIndex)
      .flatMap(s => s.map{case (copy, index) => removeCharAtIndex(copy, index)})

    val funnelWords = oneLetterFunneledWords ++ twoLetterFunnelWords

    funnelWords.filter( funneledWord => dictionary.contains(funneledWord))
  }


  private def removeCharAtIndex(copy: String, index: Int) = {
    copy.substring(0, index) + copy.substring(index + 1, copy.length)
  }

}
