package cryptanalysis.vigenere

import cryptanalysis.Breaker
import cryptanalysis.caesar.CaesarBreaker
import cryptanalysis.language.Language

import scala.annotation.tailrec

object VigenereBreaker extends Breaker[String] {

  override def break(cipherText: String, language: Language): Seq[(String, String)] = {

    val normalizedCipherText = language.normalizeString(cipherText)
    val normalizedCipherTextWithoutSpaces = language.normalizeStringWithoutSpaces(cipherText)
    val possibleRepetitions = getAllPossibleConsecutiveSubstrings(normalizedCipherTextWithoutSpaces)

    val distanceBetweenSubstrings= possibleRepetitions
      .map(possibleKey => getDistanceBetweenSubstrings(normalizedCipherTextWithoutSpaces, possibleKey))
      .filter(x => x > 1)

    val keyDistances = distanceBetweenSubstrings.flatMap(factorize(_))

    val modes: Iterable[Int] = getModes(keyDistances)
    val decipherAttempts = for {
      mode <- modes
      keyCharacters = (1 to mode).mkString * cipherText.length
      keyLengthColumns: Seq[(Char, Char)] = keyCharacters.zip(normalizedCipherText)

      keyLengthStrings = getKeyLengthStrings(mode, keyLengthColumns)

      brokenKeyLengthStrings = keyLengthStrings.map(str => CaesarBreaker.break(str, language)).transpose

      combinedBrokenKeyLengthStrings = zipBrokenKeyLengthStrings(brokenKeyLengthStrings)

      keys = breakKeys(brokenKeyLengthStrings, language)
    } yield keys.zip(combinedBrokenKeyLengthStrings)

    decipherAttempts.flatten.toSeq
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
    (3 to cipherText.length).flatMap(i => cipherText.sliding(i, 1)).distinct
  }

  private[vigenere] def getDistanceBetweenSubstrings(string: String, subString: String): Int = {
    val index1 = string.indexOfSlice(subString)
    if (index1 + subString.length > string.length) {
      -1
    }
    else {
      val stringWithoutFirstOccurrenceOfSubstring = string.substring(index1 + subString.length)
      val index2 = stringWithoutFirstOccurrenceOfSubstring.indexOfSlice(subString)
      if (index2 < 0)
        -1
      else {
        subString.length + index2
      }
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
