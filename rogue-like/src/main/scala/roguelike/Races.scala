package roguelike

import roguelike.Weapons.{Axe, Scimitar, ShortSword, Weapon}

object Races{

trait Race extends Displayable{
  val hitPoints: Int
  val amourClass: Int
  val weapon: Weapon
}

trait Gold {
  val gold: Int
}

case class Human(hitPoints: Int = 16, amourClass: Int = 13, weapon: Weapon = new Axe(), gold: Int = 0) extends Race with Gold{
  def displayName: String = "Human"
}

case class Goblin(hitPoints: Int = 4, amourClass: Int = 10, weapon: Weapon = new Scimitar()) extends Race{
  def displayName: String = "Goblin"
}

case class HobGoblin(hitPoints: Int = 6, amourClass: Int = 10, weapon: Weapon = new ShortSword()) extends Race{
  def displayName: String = "Hob Goblin"
}

}
