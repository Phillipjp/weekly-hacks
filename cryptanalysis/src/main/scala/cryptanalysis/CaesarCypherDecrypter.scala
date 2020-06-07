package cryptanalysis

class CaesarCypherDecrypter(sampleText: String) {

  private val letterFrequencies = getLetterFrequencies(sampleText)

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
//    val sortedRankedFrequencies = frequencies.sortBy(-_._2)
      sortedRankedFrequencies.map(_._1)
//    rankedFrequencies

  }

  private def sortByCharacterFrequency(a: (Char, Int), b: (Char, Int)): Boolean = {
    if(a._2 == b._2)
      a._1 < b._1
    else
      a._2 > b._2

  }

}
