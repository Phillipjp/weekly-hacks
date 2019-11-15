package roguelike

import roguelike.Races._
import roguelike.Dice._
import roguelike.Statuses.{Burnt, Frozen, Shocked}

import scala.annotation.tailrec
import scala.util.Random

object Game extends App {


  playGame()

  def playGame(): Unit = {
    val player = Human()
    println(s"You entered the dungeon as a ${player.displayName} wielding a ${player.weapon.displayName}")
    turn(player, getOpponent)

  }

  @tailrec
  def turn(player: Human, opponent: Race): Unit = {

    val (postAttackPlayer: Race, attackedOpponent: Race) = attackWithStatusEffect(player, opponent)

    if (attackedOpponent.hitPoints < 1) {
      endTurn(postAttackPlayer.asInstanceOf[Human], attackedOpponent)
    }
    else {
      val (postAttackOpponent, attackedPlayer) = attackWithStatusEffect(attackedOpponent, postAttackPlayer.asInstanceOf[Human])

      if (attackedPlayer.hitPoints < 1) {
        println(s"Game over! You died with ${player.gold} gold pieces")
      }
      else {
        turn(attackedPlayer.asInstanceOf[Human], postAttackOpponent)
      }
    }

  }

  private def attackWithStatusEffect(attacker: Race, defender: Race) = {
    attacker.status match {
      case _: Shocked =>
        (attacker.statusEffect(), defender)
      case _ =>
        val attackedOpponent = attack(attacker, defender)
        val statusOpponent = attackedOpponent.status match {
          case _: Burnt => attackedOpponent.statusEffect()
          case _: Frozen => attackedOpponent.statusEffect()
          case _ => attackedOpponent
        }
        (attacker, statusOpponent)
    }
  }

  private def endTurn(player: Human, opponent: Race): Unit = {
    println(s"You defeated the ${opponent.displayName}.")
    val updatedPlayed = player.loot(opponent)
    val healedPlayer = healPlayer(updatedPlayed)
    println("Do you wish to continue into the dungeon? (y/n)")
    val carryOn = scala.io.StdIn.readLine().toLowerCase
    if (carryOn.equals("y"))
      turn(healedPlayer, getOpponent)
    else
      println(s"You survived the dungeon with ${healedPlayer.gold} gold pieces")

  }

  private def healPlayer(player: Human): Human = {
    println(s"You have ${player.hitPoints}/${Races.humanHitPoints} hit points remaining and ${player.healingPotions} healing potions. Do you wish to heal? (y/n)")
    val heal = scala.io.StdIn.readLine().toLowerCase
    heal match {
      case "y" => player.heal()
      case _ => player
    }
  }

  private def attack(attacker: Race, defender: Race): Race = {
    val armourRoll = rollDice(1, 20)
    val canAttack = armourRoll + attacker.weapon.toHit > defender.amourClass
    val affinityAttackMultiplier = defender.elementalAffinity.elementalAffinityDamageMultiplier(attacker.weapon.element)
    val attackDamage = attacker.attack() * affinityAttackMultiplier
    defender match {
      case goblin: Goblin if canAttack =>
        println(s"You dealt $attackDamage damage to the ${defender.displayName}")
        goblin.copy(hitPoints = defender.hitPoints - attackDamage, status = attacker.weapon.specialAttack())
      case hobGoblin: HobGoblin if canAttack =>
        println(s"You dealt $attackDamage damage to the ${defender.displayName}")
        hobGoblin.copy(hitPoints = defender.hitPoints - attackDamage, status = attacker.weapon.specialAttack())
      case orc: Orc if canAttack =>
        println(s"You dealt $attackDamage damage to the ${defender.displayName}")
        orc.copy(hitPoints = defender.hitPoints - attackDamage, status = attacker.weapon.specialAttack())
      case human: Human if canAttack =>
        println(s"The ${attacker.displayName} dealt $attackDamage damage to you")
        human.copy(hitPoints = defender.hitPoints - attackDamage)
      case human: Human =>
        println(s"The ${attacker.displayName}'s attack missed")
        human
      case enemy: Race =>
        println("Your attack missed")
        enemy
    }
  }

  private def getOpponent: Race = {
    val prob = Random.nextInt(100)
    val enemy = prob match {
      case _ if prob < 33 =>
        Goblin()
      case _ if prob < 66 =>
        HobGoblin()
      case _ if prob < 100 =>
        Orc()
    }
    println(s"A wild ${enemy.displayName} appeared wielding a ${enemy.weapon.displayName}.")
    enemy

  }
}
