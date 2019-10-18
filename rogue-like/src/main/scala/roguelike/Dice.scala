package roguelike

import scala.util.Random

object Dice {

  def rollDice(numberOfDie: Int, sizeOfDie: Int)(): Int = {
    (0 until numberOfDie).map(i => Random.nextInt(sizeOfDie)+1).sum
  }

}
