package roguelike

import roguelike.Races.{Goblin, HobGoblin, Human, Race}
import roguelike.Dice._

import scala.annotation.tailrec
import scala.util.Random

object Game extends App{


  playGame()

  def playGame(): Unit = {

    turn(Human(), getOpponent)

  }

  @tailrec
  def turn(player: Human, opponent: Race): Unit = {

    val attackedOpponent = attack(player, opponent)

    if(attackedOpponent.hitPoints < 1){
      endTurn(player, attackedOpponent)
    }
    else{
      val attackedPlayer = attack(attackedOpponent, player).asInstanceOf[Human]
      if(attackedPlayer.hitPoints < 1){
        println(s"Game over! You died with ${player.gold} gold pieces")
      }
      else{
        turn(attackedPlayer, attackedOpponent)
      }
    }

  }

  private def endTurn(player: Human, opponent: Race): Unit = {
    println(s"You defeated the ${opponent.displayName}")
    println(s"You have ${player.hitPoints} hit points remaining")
    println("Do you wish to continue into the dungeon? (y/n)")
    val input = scala.io.StdIn.readLine().toLowerCase
    val gold = rollDice(1,3)
    val updatedPlayed = Human(player.hitPoints, player.amourClass, player.weapon, player.gold + gold)
    if(input.equals("y"))
      turn(updatedPlayed, getOpponent)
    else
      println(s"You survived the dungeon with ${updatedPlayed.gold} gold pieces")

  }

  private def attack(attacker: Race, defender: Race): Race ={
    val armourRoll = rollDice(1,20)
    if(armourRoll + attacker.weapon.toHit > defender.amourClass ){
      val attackDamage = attacker.weapon.dealDamage()
      defender match {
        case goblin: Goblin =>
          println(s"You dealt $attackDamage to the Goblin")
          goblin.copy(hitPoints = defender.hitPoints - attackDamage)
        case hobGoblin: HobGoblin =>
          println(s"You dealt $attackDamage to the Hob Goblin")
          hobGoblin.copy(hitPoints = defender.hitPoints - attackDamage)
        case human: Human =>
          println(s"The $attacker dealt $attackDamage damage to you")
          human.copy(hitPoints = defender.hitPoints - attackDamage)
      }
    }
    else {
      defender
    }
  }

  private def getOpponent: Race = {
     if (Random.nextInt(100) % 2 == 0) {
       println("A Goblin Appeared")
       Goblin()
     }
    else {
       println("A Hob Goblin Appeared")
      HobGoblin()
    }
  }
}
