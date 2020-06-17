package cryptanalysis.vigenere

import cryptanalysis.Breaker
import cryptanalysis.caesar.CaesarBreaker
import cryptanalysis.language.Language

import scala.annotation.tailrec
import scala.collection.immutable

object VigenereBreaker extends Breaker[String] {

  override def break(cipherText: String, language: Language): Seq[(String, String)] = {

    val normalizedCipherText = language.normalizeString(cipherText)
    val possibleRepetitions = getAllPossibleConsecutiveSubstrings(normalizedCipherText)

    val keyDistances = possibleRepetitions
      .map(possibleKey => getDistanceBetweenSubstrings(cipherText, possibleKey))
      .filter(_ > 1)
      .flatMap(factorize(_))

    val mode = getMode(keyDistances)

    val keyCharacters = (1 to mode).mkString * cipherText.length
    val keyLengthColumns: Seq[(Char, Char)] = keyCharacters.zip(cipherText)

    val keyLengthStrings = getKeyLengthStrings(mode, keyLengthColumns)

    val brokenKeyLengthStrings = keyLengthStrings.map(str => CaesarBreaker.break(str, language)).transpose

    val combinedBrokenKeyLengthStrings = zipBrokenKeyLengthStrings(brokenKeyLengthStrings)

    val keys = breakKeys(brokenKeyLengthStrings, language)

    keys.zip(combinedBrokenKeyLengthStrings)

  }

  private def zipBrokenKeyLengthStrings(brokenKeyLengthStrings: List[List[(Int, String)]]) = {
    brokenKeyLengthStrings.map { l =>
      val separatedCipherText = l.map { case (_, partCipherText) => partCipherText }
      zipStrings(separatedCipherText, "")
    }
  }

  private def breakKeys(brokenKeyLengthStrings: List[List[(Int, String)]], language: Language): Seq[String] = {
    brokenKeyLengthStrings.map { l =>
      val keyIndexes = l.map { case (keyIndex, _) => keyIndex }
      keyIndexes.map { key =>
        val k = key match {
          case x if x == 26 => 0
          case x => x - 1
        }
        language.alphabet.charAt(k)
      }.mkString
    }
  }

  private def getKeyLengthStrings(mode: Int, keyLengthColumns: Seq[(Char, Char)]) = {
    Stream.continually(keyLengthColumns).take(mode).zipWithIndex.map { case (cipher, index) =>
      cipher.filter { x =>
        x._1.toString == (index + 1).toString
      }.map(_._2).mkString
    }.toList
  }

  private[vigenere] def getAllPossibleConsecutiveSubstrings(cipherText: String): Seq[String] = {
    val cipherTextWithoutSpaces = cipherText.replaceAll(" ", "")
    (3 to cipherTextWithoutSpaces.length).flatMap(i => cipherTextWithoutSpaces.sliding(i, 1)).distinct
  }

  private[vigenere] def getDistanceBetweenSubstrings(string: String, subString: String): Int = {
    val index1 = string.indexOfSlice(subString)
    if (index1 + subString.length > string.length) {
      -1
    }
    else {
      val stringWithoutFirstOccurrenceOfSubstring = string.substring(index1 + subString.length)
      val index2 = stringWithoutFirstOccurrenceOfSubstring.indexOfSlice(subString)
      subString.length + index2
    }
  }

  // get all factors of a number excluding 1 and 2
  @tailrec
  private[vigenere] def factorize(num: Int, x: Int = 3, factors: Seq[Int] = Seq()): Seq[Int] = {
    if (x >= num)
      factors :+ num
    else {
      if (num % x == 0)
        factorize(num, x + 1, factors :+ x)
      else
        factorize(num, x + 1, factors)
    }
  }

  private[vigenere] def getModes(list: Seq[Int]): Iterable[Int] = {
    val grouped = list.groupBy(x => x).mapValues(_.size)
    val modeValue = grouped.maxBy(_._2)._2
    grouped.filter(_._2 == modeValue).keys
  }

  private[vigenere] def getMode(list: Seq[Int]): Int = {
    list.groupBy(x => x).mapValues(_.size).maxBy(_._2)._1
  }

  @tailrec
  private[vigenere] final def zipStrings(strings: Seq[String], string: String): String = {
    if (strings.isEmpty)
      string
    else {
      val end = strings.map(str => str.head).mkString
      val newStrings = strings.map(_.tail).filter(!_.isEmpty)
      zipStrings(newStrings, string + end)
    }
  }
}
