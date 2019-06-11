import scala.util.Random
import WordFilter._

trait Player {

  def makeGuess(options: Seq[String]) : Char
}

class HumanPlayer extends Player{
  override def makeGuess(options: Seq[String]): Char = {
    val guess = getInput.toLowerCase
    if(options.contains(guess) && guess.matches("[a-z]") && guess.length == 1){
      guess.charAt(0)
    }else{
      println("Please guess a single letter you haven't already guessed")
      makeGuess(options)
    }
  }

  private def getInput: String = {
    scala.io.StdIn.readLine()
  }


}

case class ComputerPLayer(rand: Random) extends Player{
  override def makeGuess(options: Seq[String]): Char = {
    options(rand.nextInt(options.length)).charAt(0)
  }
}

case class LetterFrequencyComputerPlayer() extends Player{

  override def makeGuess(options: Seq[String]): Char = {
    options.head.head
  }
}

case class WordDictionaryComputerPlayer(rand: Random) extends Player{
  override def makeGuess(options: Seq[String]): Char = ???
}