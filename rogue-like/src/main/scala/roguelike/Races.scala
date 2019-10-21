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

  case class Human(hitPoints: Int = 16, amourClass: Int = 13, weapon: Weapon = Axe(), gold: Int = 0) extends Race with Gold{

    def attack(): Int = weapon.dealDamage

    def displayName: String = "Human"
  }

  case class Goblin(hitPoints: Int = 4, amourClass: Int = 10, weapon: Weapon = Scimitar()) extends Race{

    def attack(): Int = weapon.dealDamage

    def displayName: String = "Goblin"
  }

  case class HobGoblin(hitPoints: Int = 6, amourClass: Int = 10, weapon: Weapon = ShortSword()) extends Race{

    def attack(): Int = weapon.dealDamage

    def displayName: String = "Hob Goblin"
  }

  case class Orc (hitPoints: Int = 8, amourClass: Int = 12, weapon: Weapon = Sword()) extends Race{

    def attack(): Int = weapon.dealDamage

    def displayName: String = "Orc"

  }

}
