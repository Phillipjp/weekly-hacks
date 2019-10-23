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

  trait Healer {
    val healingPotions: Int

    def heal(): Human
  }

  trait Looter {

    def loot(enemy: Race): Race
  }

  private val humanHitPoints = 16

  case class Human(hitPoints: Int = humanHitPoints, amourClass: Int = 13, weapon: Weapon = Axe(), gold: Int = 0, healingPotions: Int = 1) extends Race with Healer with Looter {

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
      println(s"Do you want to take the ${enemy.displayName}'s ${enemy.weapon.displayName} instead of your ${this.weapon.displayName}? (y/n)")
      val lootWeapon = scala.io.StdIn.readLine().toLowerCase
      val newWeapon = if(lootWeapon.equals("y"))
        enemy.weapon
      else
        this.weapon

      val lootedGold = rollDice(1,3)
      enemy match {
        case _: Orc if rollDice(1,4) == 1 =>
          println(s"Found $lootedGold gold piece(s) and 1 healing potion.")
          this.copy(weapon = newWeapon, gold = gold + lootedGold, healingPotions = healingPotions +1)
        case _ =>
          println(s"Found $lootedGold gold piece(s).")
          this.copy(weapon = newWeapon, gold = gold + lootedGold)
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
