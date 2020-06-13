package cryptanalysis.caesar

import cryptanalysis.Breaker
import cryptanalysis.language.Language

import scala.annotation.tailrec

object CaesarBreaker extends Breaker[Int]{


  override def break(cipherText: String, language: Language): Seq[(Int, String)] = {
    val normalizedCipherText = language.normalizeStringWithoutSpaces(cipherText)
    val expectedLetterFrequencies = calculateExpectedLetterFrequencies(normalizedCipherText, language.letterProbabilities)

    val initialFrequencies = language.alphabet.map(letter => (letter, 1)).toMap

    val shiftChiSquareScores = (0 until language.alphabet.length).map(shift => getChiSquaredScoreForShift(cipherText, expectedLetterFrequencies, initialFrequencies, shift, language))

    val closestShifts = shiftChiSquareScores.zipWithIndex.sortBy(_._1).take(5).map(_._2)

    closestShifts.map{shift => (language.alphabet.length - shift, CaesarCipher.encrypt(cipherText, shift, language))}
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

  private def calculateExpectedLetterFrequencies(normalizedCipherText: String, letterProbabilities: Seq[Double]): Seq[Double] = {
    val textLength = normalizedCipherText.length
    letterProbabilities.map(x => x * textLength)
  }

}
