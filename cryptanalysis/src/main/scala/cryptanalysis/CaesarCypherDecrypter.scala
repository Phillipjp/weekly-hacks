package cryptanalysis

class CaesarCypherDecrypter(sampleText: String) {

  private val letterFrequencies = getLetterFrequencies(sampleText)
  private val bigramFrequencies = getNgramFrequencies(sampleText, 2)
  private val triramFrequencies = getNgramFrequencies(sampleText, 3)

  private[cryptanalysis] def getLetterFrequencies(text: String): Seq[Char] = {
    val normalizedText = Utils.normalizeString(text).replaceAll(" ", "")

    val initialFrequencies = Map(
      'A' -> 0, 'B' -> 0, 'C' -> 0, 'D' -> 0, 'E' -> 0, 'F' -> 0, 'G' -> 0, 'H' -> 0, 'I' -> 0,
      'J' -> 0, 'K' -> 0, 'L' -> 0, 'M' -> 0, 'N' -> 0, 'O' -> 0, 'P' -> 0, 'Q' -> 0, 'R' -> 0,
      'S' -> 0, 'T' -> 0, 'U' -> 0, 'V' -> 0, 'W' -> 0, 'X' -> 0, 'Y' -> 0, 'Z' -> 0
    )

    def getFrequencies(sampleText: String, frequencies: Map[Char, Int]): Map[Char, Int] = {
      if (sampleText.isEmpty) {
        frequencies
      }
      else {
        val currentChar = sampleText.head
        val frequency = frequencies(currentChar) + 1
        getFrequencies(sampleText.tail, frequencies + (currentChar -> frequency))
      }
    }

    val frequencies: Seq[(Char, Int)] = getFrequencies(normalizedText, initialFrequencies).toSeq
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
    val bigrams = normalizedText
      .split(" ")
      .flatMap(word => word.sliding(n, 1))
      .groupBy(gram => gram)
      .toList
      .map(gram => (gram._1, gram._2.length))
      .sortBy(-_._2)
      .map(_._1)

    bigrams

  }

  def decryptCypherText(cypherText: String): String = {
    val normalizedCypherText = Utils.normalizeString(cypherText)
    val cypherCharactersOrderedByFreequncy = getLetterFrequencies(normalizedCypherText.replaceAll(" ", ""))
    val characterMappings = cypherCharactersOrderedByFreequncy.zip(letterFrequencies).toMap + (' ' -> ' ')

    cypherText.map(char => characterMappings(char))
  }

}
