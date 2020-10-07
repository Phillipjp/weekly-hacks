package smorsecode

import scala.io.Source

object Main {

  def main(args: Array[String]): Unit = {
    val source = Source.fromResource("enable1.txt")
    val wordList = source.getLines().toList

    val answerOne = challengeOne(wordList, 13)
    println("CHALLENGE ONE")
    answerOne.foreach(println)
    println()

    val answerTwo = challengeTwo(wordList, "---------------")
    println("CHALLENGE TWO")
    answerTwo.foreach(println)
    println()

    val answerThree = challengeThree(wordList, 21)
    println("CHALLENGE THREE")
    answerThree.foreach(println)
    println()

    val answerFour = challengeFour(wordList, 13)
    println("CHALLENGE FOUR")
    answerFour.foreach(println)
    println()

    val answerFive = challengeFive(wordList)
    println("CHALLENGE FIVE")
    answerFive.foreach(println)
    println()
  }

  def challengeOne(wordList: Seq[String], nWords: Int): Seq[(String, Seq[String])] = {
    wordList.map(word => (word, Smorse.smorse(word)))
      .groupBy(_._2)
      .toSeq
      .filter(_._2.length == nWords)
      .map { case (smorse, wordSmorseList) => (smorse, wordSmorseList.map(_._1)) }
  }

  def challengeTwo(wordList: Seq[String], subString: String): Seq[String] = {
    wordList.map(word => (word, Smorse.smorse(word)))
      .filter { case (_, smorse) => smorse.contains(subString) }
      .map(_._1)
  }

  private def filterWordListByWordLengthAndSecondCondition(wordList: Seq[String], wordLength: Int, condition: String => Boolean): Seq[String] = {
    wordList
      .filter(_.length == wordLength)
      .map(word => (word, Smorse.smorse(word)))
      .filter { case (_, smorse) => condition(smorse) }
      .map(_._1)
  }

  def challengeThree(wordList: Seq[String], wordLength: Int): Seq[String] = {
    val condition = (smorse: String) => {
      val dots = smorse.filter(_ == '.').length
      val dashes = smorse.filter(_ == '-').length
      dots == dashes
    }
    filterWordListByWordLengthAndSecondCondition(wordList, wordLength, condition)
  }

  def challengeFour(wordList: Seq[String], wordLength: Int): Seq[String] = {
    val condition = (smorse: String) => {smorse == smorse.reverse}
    filterWordListByWordLengthAndSecondCondition(wordList, wordLength, condition)
  }

  def challengeFive(wordList: Seq[String]): Set[String] = {
    val allPermutations =
      Seq("-------------", "------------.", "-----------..", "----------...", "---------....", "--------.....", "-------......",
        "------.......", "-----........", "----.........", "---..........", "--...........", "-............", ".............")
        .flatMap(_.permutations)
        .toSet

    val existingPermutations = wordList.map(Smorse.smorse)
      .filter(_.length > 12)
      .flatMap(smorse => smorse.sliding(13, 1).filter(_.length > 12))
      .toSet

    allPermutations -- existingPermutations

  }
}
