package roguelike

import roguelike.Races._
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
    val gold = rollDice(1,3)
    println(s"You defeated the ${opponent.displayName}. You looted $gold gold piece(s) from the body.")

    val healedPlayer = healPlayer(player)

    println("Do you wish to continue into the dungeon? (y/n)")
    val carryOn = scala.io.StdIn.readLine().toLowerCase
    val updatedPlayed = healedPlayer.copy(gold = healedPlayer.gold + gold)
    if(carryOn.equals("y"))
      turn(updatedPlayed, getOpponent)
    else
      println(s"You survived the dungeon with ${updatedPlayed.gold} gold pieces")

  }

  private def healPlayer(player: Human): Human = {
    println(s"You have ${player.hitPoints} hit points remaining. Do you wish to heal?")
    val heal = scala.io.StdIn.readLine().toLowerCase
    heal match {
      case "y" => player.heal()
      case _ => player
    }
  }

  private def attack(attacker: Race, defender: Race): Race ={
    val armourRoll = rollDice(1,20)
    if(armourRoll + attacker.weapon.toHit > defender.amourClass ){
      val attackDamage = attacker.attack
      defender match {
        case goblin: Goblin =>
          println(s"You dealt $attackDamage damage to the ${defender.displayName}")
          goblin.copy(hitPoints = defender.hitPoints - attackDamage)
        case hobGoblin: HobGoblin =>
          println(s"You dealt $attackDamage damage to the ${defender.displayName}")
          hobGoblin.copy(hitPoints = defender.hitPoints - attackDamage)
        case orc: Orc =>
          println(s"You dealt $attackDamage damage to the ${defender.displayName}")
          orc.copy(hitPoints = defender.hitPoints - attackDamage)
        case human: Human =>
          println(s"The ${attacker.displayName} dealt $attackDamage damage to you")
          human.copy(hitPoints = defender.hitPoints - attackDamage)
      }
    }
    else {
      attacker match {
        case _: Human =>
          println("Your attack missed")
        case enemy: Race =>
          println(s"The ${enemy.displayName}'s attack missed")
      }
      defender
    }
  }

  private def getOpponent: Race = {
    val prob = Random.nextInt(100)
     if (prob < 33) {
       println("A Goblin Appeared")
       Goblin()
     }
    else if(prob < 66){
       println("A Hob Goblin Appeared")
      HobGoblin()
    }
    else{
       println("An Orc Appeared")
       Orc()
     }
  }
}
