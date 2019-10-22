package roguelike

import roguelike.Weapons._

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

  val humanHitPoints = 16

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

  }

  case class Goblin(hitPoints: Int = 4, amourClass: Int = 10, weapon: Weapon = Scimitar()) extends Race{

    def attack(): Int = weapon.dealDamage()

    def displayName: String = "Goblin"
  }

  case class HobGoblin(hitPoints: Int = 6, amourClass: Int = 10, weapon: Weapon = ShortSword()) extends Race{

    def attack(): Int = weapon.dealDamage()

    def displayName: String = "Hob Goblin"
  }

  case class Orc (hitPoints: Int = 8, amourClass: Int = 12, weapon: Weapon = Sword()) extends Race{

    def attack(): Int = weapon.dealDamage

    def displayName: String = "Orc"

  }

}
