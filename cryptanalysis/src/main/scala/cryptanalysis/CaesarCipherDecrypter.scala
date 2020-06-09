package cryptanalysis

import org.apache.commons.math3.stat.inference.ChiSquareTest

import scala.annotation.tailrec

object CaesarCipherDecrypter {

  private val letterProbabilities = Seq(8.12, 1.49, 2.71, 4.32, 12.02, 2.3, 2.03, 5.92, 7.31, 0.1, 0.69, 3.98, 2.61,
    6.95, 7.68, 1.82, 0.11, 6.02, 6.28, 9.1, 2.88, 1.11, 2.09, 0.17, 2.11, 0.07)

  @tailrec
  private [cryptanalysis] def getFrequencies(sampleText: String, frequencies: Map[Char, Int]): Map[Char, Int] = {
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

  def breakCipher(cypherText: String): Seq[String] = {
    val normalizedCypherText = Utils.normalizeStringWithoutSpaces(cypherText)
    val expectedLetterFrequencies: _root_.scala.Array[Double] = calculateExpectedLetterFrequencies(normalizedCypherText)

    val initialFrequencies = Map(
      'A' -> 0, 'B' -> 0, 'C' -> 0, 'D' -> 0, 'E' -> 0, 'F' -> 0, 'G' -> 0, 'H' -> 0, 'I' -> 0,
      'J' -> 0, 'K' -> 0, 'L' -> 0, 'M' -> 0, 'N' -> 0, 'O' -> 0, 'P' -> 0, 'Q' -> 0, 'R' -> 0,
      'S' -> 0, 'T' -> 0, 'U' -> 0, 'V' -> 0, 'W' -> 0, 'X' -> 0, 'Y' -> 0, 'Z' -> 0
    )

    val shiftChiSquareScores = (0 to 25).map(shift => getChiSquaredScoreForShift(cypherText, expectedLetterFrequencies, initialFrequencies, shift))

    val closestShifts = shiftChiSquareScores.zipWithIndex.sortBy(_._1).take(5).map(_._2)

    closestShifts.map{shift => new CaesarCipherEncrypter(shift).encrypt(cypherText)}
  }


  private def getChiSquaredScoreForShift(cypherText: String, expectedLetterFrequencies: Array[Double], initialFrequencies: Map[Char, Int], shift: Int): Double = {
    val decipheredText = new CaesarCipherEncrypter(shift).encrypt(cypherText)
    val decipheredTextLetterFrequency = getFrequencies(Utils.normalizeStringWithoutSpaces(decipheredText), initialFrequencies)
      .toArray
      .sortBy { case (letter, _) => letter }
      .map { case (_, frequency) => frequency.toLong }
    new ChiSquareTest().chiSquare(expectedLetterFrequencies, decipheredTextLetterFrequency)
  }

  private [cryptanalysis] def calculateExpectedLetterFrequencies(normalizedCypherText: String): Array[Double] = {
    val textLength = normalizedCypherText.length
    letterProbabilities.map(x => x * textLength).toArray
  }

  def decrypt(cypherText: String, shift: Int): String = {
    new CaesarCipherEncrypter(26 - shift).encrypt(cypherText)
  }
}
