import scala.util.Random
import WordFilter._

case class Result(result: String, guess: String, answer: String, lives: Int)

class Hangman {

  val OPTIONS: Seq[String] = "abcdefghijklmnopqrstuvwxyz".toSeq.map(_.toString())

  def pickRandomWord(words: Seq[String], random: Random): String = {
    words(random.nextInt(words.length))
  }

  def guessCharacter(options: Seq[String], rand: Random): Char = {
    options(rand.nextInt(options.length)).charAt(0)
  }


  def hangman(words: Seq[String], rand: Random, options: Seq[String], player: Player): Unit ={

    val hiddenWord = pickRandomWord(words, rand)
    val filteredWords = filterWords(filterWords(words, exactWordFilter(hiddenWord)), wordLengthFilter(hiddenWord.length))
    val answer = Stream.continually("_").take(hiddenWord.length).mkString

    def playTurn(hiddenWord: String, answer: String, words: Seq[String], options: Seq[String], lives: Int, rand: Random, player: Player): Result = {
      if(answer == hiddenWord){
        Result("WIN", answer, hiddenWord, lives)
      } else if(lives == 0){
        Result("LOSE", answer, hiddenWord, lives)
      } else{
        val guess = makeGuess(answer, options, lives, player)
        val filteredOptions = filterWords(options, exactWordFilter(guess.toString))

        if(hiddenWord.contains(guess)){
          if(suitableReplacementExists(guess, words)){
            val filteredWords = filterWords(words, doesNotContainCharacterFilter(guess))
            val newHiddenWord = filteredWords(rand.nextInt(filteredWords.length))
            printHangMan(lives - 1)
            playTurn(newHiddenWord, answer, filteredWords, filteredOptions, lives - 1, rand, player)
          } else {
            val indexes = 0.until(hiddenWord.length).filter(hiddenWord.charAt(_) == guess)
            val filteredWords = keepWordsWithGuessCharacterAtIndexes(words, indexes, guess)
            val newAnswer = updateAnswer(answer, guess, indexes)
            printHangMan(lives)
            playTurn(hiddenWord, newAnswer, filteredWords, filteredOptions, lives, rand, player)
          }
        } else {
          val filteredWords = filterWords(words, doesNotContainCharacterFilter(guess))
          printHangMan(lives - 1)
          playTurn(hiddenWord, answer, filteredWords, filteredOptions, lives - 1, rand, player)
        }
      }
    }
    val result = playTurn(hiddenWord, answer, filteredWords, options, 10, rand, player)
    println(s"You ${result.result}! GUESS: ${result.guess} ANSWER: ${result.answer} LIVES: ${result.lives}")
  }

  private def makeGuess(answer: String, options: Seq[String], lives: Int, player: Player) = {
    println(s"LIVES: $lives")
    answer.foreach(c => print(s" $c "))
    println()
    println("Make a guess")
    val guess = player.makeGuess(options)
    println(s"GUESS: $guess")
    guess
  }

  def updateAnswer(answer: String, guess: Char, indexes: Seq[Int]): String = {
    answer
      .zipWithIndex
      .map(c => {
        if(indexes.contains(c._2)){
          guess
        }
        else {
          c._1
        }
      })
      .mkString
  }

  def suitableReplacementExists(guess: Char, words: Seq[String]): Boolean = {
    filterWords(words, doesNotContainCharacterFilter(guess)).nonEmpty
  }

  def printHangMan(lives: Int): Unit ={
    val hangman = lives match  {
      case 9 =>
        """
          |
          |
          |
          |
          |
          |____
        """.stripMargin
      case 8 =>
        """
          |
          |  |
          |  |
          |  |
          |  |
          |__|__
        """.stripMargin
      case 7 =>
        """
          |   ______
          |  |
          |  |
          |  |
          |  |
          |__|__
        """.stripMargin
      case 6 =>
        """
          |   ______
          |  |      |
          |  |
          |  |
          |  |
          |__|__
        """.stripMargin
      case 5 =>
        """
          |   ______
          |  |      |
          |  |      O
          |  |
          |  |
          |__|__
        """.stripMargin
      case 4 =>
        """
          |   ______
          |  |      |
          |  |      O
          |  |      |
          |  |
          |__|__
        """.stripMargin
      case 3 =>
        """
          |   ______
          |  |      |
          |  |      O
          |  |      |-
          |  |
          |__|__
        """.stripMargin
      case 2 =>
        """
          |   ______
          |  |      |
          |  |      O
          |  |     -|-
          |  |
          |__|__
        """.stripMargin
      case 1 =>
        """
          |   ______
          |  |      |
          |  |      O
          |  |     -|-
          |  |       \
          |__|__
        """.stripMargin
      case 0 =>
        """
          |   ______
          |  |      |
          |  |      O
          |  |     -|-
          |  |     / \
          |__|__
        """.stripMargin
    }
    println(hangman)
  }


}

object Hangman{

  def main(args: Array[String]): Unit = {
    val hangman = new Hangman
    val words = Util.readInVocab[String]("src/main/resources/words.txt")
    val rand = new Random()
    val computerPlayer = LetterFrequencyComputerPlayer()
    val humanPlayer = new HumanPlayer()
    val options =  Seq('e', 't', 'a', 'o', 'i', 'n', 's', 'h', 'r', 'd', 'l', 'c', 'u', 'm', 'w', 'f', 'g', 'y', 'p', 'b', 'v', 'k', 'j', 'x', 'q', 'z')
        .map(_.toString)

    hangman.hangman(words, rand, options, computerPlayer)
  }
}