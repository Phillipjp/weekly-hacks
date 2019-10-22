package roguelike

import roguelike.Weapons._
import roguelike.Dice.rollDice

object Races{

  sealed trait Race extends Displayable{
    val hitPoints: Int
    val amourClass: Int
    val weapon: Weapon

    def attack() : Int
  }

  trait Gold {
    val gold: Int
  }

  trait Healer {
    val healingPotions: Int

    def heal(): Human
  }

  private val humanHitPoints = 16

  case class Human(hitPoints: Int = humanHitPoints, amourClass: Int = 13, weapon: Weapon = Axe(), gold: Int = 0, healingPotions: Int = 1) extends Race with Gold with Healer {

    def attack(): Int = weapon.dealDamage()

    def displayName: String = "Human"

    def heal(): Human = {
      if(healingPotions > 0) {
        println(s"Drinking a healing potion. You have ${healingPotions-1} healing potions left.")
        this.copy(hitPoints = humanHitPoints, healingPotions = healingPotions - 1)
      }
      else{
        println("Can't heal. You're out of healing potions!")
        this
      }
    }

    def loot(enemy: Race): Human = {
      val lootedGold = rollDice(1,3)
      enemy match {
        case _: Orc if rollDice(1,4) == 1 =>
          println(s"Found $lootedGold gold piece(s) and 1 healing potion.")
          this.copy(gold = gold + lootedGold, healingPotions = healingPotions +1)
        case _ =>
          println(s"Found $lootedGold gold piece(s).")
          this.copy(gold = gold + lootedGold)
      }
    }

  }

  case class Goblin(hitPoints: Int = rollDice(1,4), amourClass: Int = 10, weapon: Weapon = Scimitar()) extends Race{

    def attack(): Int = weapon.dealDamage()

    def displayName: String = "Goblin"
  }

  case class HobGoblin(hitPoints: Int = rollDice(1,6), amourClass: Int = 10, weapon: Weapon = ShortSword()) extends Race{

    def attack(): Int = weapon.dealDamage()

    def displayName: String = "Hob Goblin"
  }

  case class Orc (hitPoints: Int = rollDice(1,10), amourClass: Int = 12, weapon: Weapon = Sword()) extends Race{

    def attack(): Int = weapon.dealDamage()

    def displayName: String = "Orc"

  }

}
