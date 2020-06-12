package cryptanalysis.caesar

import cryptanalysis.Breaker
import cryptanalysis.language.Language

import scala.annotation.tailrec

object CaesarBreaker extends Breaker[Int]{

  // Sourced from: http://pi.math.cornell.edu/~mec/2003-2004/cryptography/subs/frequencies.html
  private val letterProbabilities = Seq(8.12, 1.49, 2.71, 4.32, 12.02, 2.3, 2.03, 5.92, 7.31, 0.1, 0.69, 3.98, 2.61,
    6.95, 7.68, 1.82, 0.11, 6.02, 6.28, 9.1, 2.88, 1.11, 2.09, 0.17, 2.11, 0.07)


  override def break(cipherText: String, language: Language): Seq[(Int, String)] = {
    val normalizedCipherText = language.normalizeStringWithoutSpaces(cipherText)
    val expectedLetterFrequencies = calculateExpectedLetterFrequencies(normalizedCipherText)

    val initialFrequencies = language.alphabet.map(letter => (letter, 1)).toMap

    val shiftChiSquareScores = (0 until language.alphabet.size).map(shift => getChiSquaredScoreForShift(cipherText, expectedLetterFrequencies, initialFrequencies, shift, language))

    val closestShifts = shiftChiSquareScores.zipWithIndex.sortBy(_._1).take(5).map(_._2)

    closestShifts.map{shift => (language.alphabet.size - shift, CaesarCipher.encrypt(cipherText, shift, language))}
  }

  private def getChiSquaredScoreForShift(cipherText: String, expectedLetterFrequencies: Seq[Double], initialFrequencies: Map[Char, Int], shift: Int, language: Language): Double = {
    val decipheredText = CaesarCipher.encrypt(cipherText, shift, language)
    val decipheredTextLetterFrequency = getFrequencies(language.normalizeStringWithoutSpaces(decipheredText), initialFrequencies)
      .toSeq
      .sortBy { case (letter, _) => letter }
      .map { case (_, frequency) => frequency }
    chiSquare(decipheredTextLetterFrequency, expectedLetterFrequencies)
  }

  private [caesar] def chiSquare(observed: Seq[Int], expected: Seq[Double]): Double = {
    if(observed.length != expected.length)
      throw new Exception(s"length of observed frequencies (${observed.length}) does not match the length of expected frequencies (${expected.length})")
    else
      observed.zip(expected).map{case(ob, ex) => ((ob-ex)*(ob-ex))/ex}.sum
  }

  @tailrec
  private [caesar] def getFrequencies(sampleText: String, frequencies: Map[Char, Int]): Map[Char, Int] = {
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

  private def calculateExpectedLetterFrequencies(normalizedCipherText: String): Seq[Double] = {
    val textLength = normalizedCipherText.length
    letterProbabilities.map(x => x * textLength)
  }

}
