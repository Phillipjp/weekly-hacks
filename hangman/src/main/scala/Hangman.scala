import scala.util.Random

case class Result(result: String, guess: String, answer: String, lives: Int)

class Hangman {

  val OPTIONS: Seq[String] = "abcdefghijklmnopqrstuvwxyz".toSeq.map(_.toString())

  def pickRandomWord(words: Seq[String], random: Random): String = {
    words(random.nextInt(words.length))
  }

  def filterWords(words: Seq[String], condition: String => Boolean): Seq[String] ={
    words.filter(condition(_))
  }

  def exactWordFilter(word: String)(current: String): Boolean = {
    current != word
  }

  def wordLengthFilter(length: Int)(current: String): Boolean = {
    current.length == length
  }

  def characterAtIndexFilter(character: Char, index: Int)(current: String): Boolean = {
    current.charAt(index) == character
  }

  def containsCharacterFilter(character: Char)(current: String): Boolean = {
    current.contains(character)
  }
  def keepWordsWithGuessCharacterAtIndexes(words: Seq[String], indexes: Seq[Int], guess: Char): Seq[String] = {
    if(indexes.isEmpty){
      words
    }
    else{
      keepWordsWithGuessCharacterAtIndexes(filterWords(words, characterAtIndexFilter(guess, indexes.head)), indexes.drop(1), guess)
    }
  }


  def doesNotContainCharacterFilter(character: Char)(current: String): Boolean = {
    !current.contains(character)
  }

  def guessCharacter(options: Seq[String], rand: Random): Char = {
    options(rand.nextInt(options.length)).charAt(0)
  }


  def hangman(words: Seq[String], rand: Random, options: Seq[String], player: Player): Unit ={

    val hiddenWord = pickRandomWord(words, rand)
    val filteredWords = filterWords(filterWords(words, exactWordFilter(hiddenWord)), wordLengthFilter(hiddenWord.length))
    val answer = Stream.continually("-").take(hiddenWord.length).mkString

    def playTurn(hiddenWord: String, answer: String, words: Seq[String], options: Seq[String], lives: Int, rand: Random, player: Player): Result = {
      if(answer == hiddenWord){
        Result("WIN", answer, hiddenWord, lives)
      } else if(lives == 0){
        Result("LOSE", answer, hiddenWord, lives)
      } else{
        val guess = makeGuess(answer, options, lives, player)
        val filteredOptions = filterWords(options, exactWordFilter(guess.toString))

        if(hiddenWord.contains(guess)){
          if(suitableReplacement(guess, words)){
            val filteredWords = filterWords(words, doesNotContainCharacterFilter(guess))
            val newHiddenWord = filteredWords(rand.nextInt(filteredWords.length))
            playTurn(newHiddenWord, answer, filteredWords, filteredOptions, lives - 1, rand, player)
          } else {
            val indexes = 0.until(hiddenWord.length).filter(hiddenWord.charAt(_) == guess)
            val filteredWords = keepWordsWithGuessCharacterAtIndexes(words, indexes, guess)
            val newAnswer = updateAnswer(answer, guess, indexes)
            playTurn(hiddenWord, newAnswer, filteredWords, filteredOptions, lives, rand, player)
          }
        } else {
          val filteredWords = filterWords(words, doesNotContainCharacterFilter(guess))
          playTurn(hiddenWord, answer, filteredWords, filteredOptions, lives - 1, rand, player)
        }
      }
    }
    val result = playTurn(hiddenWord, answer, filteredWords, options, 10, rand, player)
    println(s"You ${result.result}! GUESS: ${result.guess} ANSWER: ${result.answer} LIVES: ${result.lives}")
  }

  private def makeGuess(answer: String, options: Seq[String], lives: Int, player: Player) = {
    println(s"LIVES: $lives")
    println(answer)
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

  def suitableReplacement(guess: Char, words: Seq[String]): Boolean = {
    filterWords(words, doesNotContainCharacterFilter(guess)).nonEmpty
  }

}

object Hangman{

  def main(args: Array[String]): Unit = {
    val hangman = new Hangman
    val words = Util.readInVocab[String]("src/main/resources/words.txt")
    val rand = new Random()
    val computer = ComputerPLayer(rand)
    val humanPlayer = new HumanPlayer()
    hangman.hangman(words, rand, hangman.OPTIONS, humanPlayer)
  }
}