package cryptanalysis

class CaesarCypherDecrypter(sampleText: String) {

  private val letterFrequencies = getLetterFrequencies(sampleText)
  private val bigramFrequencies = getNgramFrequencies(sampleText, 2)
  private val trigramFrequencies = getNgramFrequencies(sampleText, 3)


  private[cryptanalysis] def getLetterFrequencies(text: String): Seq[Char] = {
    val normalizedText = Utils.normalizeString(text).replaceAll(" ", "")


    def getFrequencies(sampleText: String, frequencies: Map[Char, Int]): Map[Char, Int] = {
      if (sampleText.isEmpty) {
        frequencies
      }
      else {

        val currentChar = sampleText.head
        if(frequencies.contains(currentChar)){
          val frequency = frequencies(currentChar) + 1
          getFrequencies(sampleText.tail, frequencies + (currentChar -> frequency))
        }
        else{
          getFrequencies(sampleText.tail, frequencies + (currentChar -> 0))
        }

      }
    }

    val frequencies: Seq[(Char, Int)] = getFrequencies(normalizedText, Map()).toSeq
    val sortedRankedFrequencies = frequencies.sortWith(sortByCharacterFrequency)
    sortedRankedFrequencies.map(_._1)
  }

  private def sortByCharacterFrequency(a: (Char, Int), b: (Char, Int)): Boolean = {
    if (a._2 == b._2)
      a._1 < b._1
    else
      a._2 > b._2
  }

  private[cryptanalysis] def getNgramFrequencies(text: String, n: Int): List[String] = {
    val normalizedText = Utils.normalizeString(text)
    val ngrams = normalizedText
      .split(" ")
      .flatMap(word => word.sliding(n, 1))
      .groupBy(gram => gram)
      .toList
      .map(gram => (gram._1, gram._2.length))
      .sortBy(-_._2)
      .map(_._1)

    ngrams

  }

  def decryptCypherText(cypherText: String): String = {
    val normalizedCypherText = Utils.normalizeString(cypherText)
    val cypherCharactersOrderedByFreequncy = getLetterFrequencies(normalizedCypherText.replaceAll(" ", ""))
    val characterMappings = cypherCharactersOrderedByFreequncy.zip(letterFrequencies).toMap + (' ' -> ' ')

    val cypherBigrams = getNgramFrequencies(normalizedCypherText, 2)
    val cypherTrigrams = getNgramFrequencies(normalizedCypherText, 3)

    normalizedCypherText.map(char => characterMappings.getOrElse(char, '?'))
  }



}
