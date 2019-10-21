package roguelike

import roguelike.Dice.rollDice


object Weapons{

sealed trait Weapon extends Displayable {

    val toHit: Int
    val diceDamage: () => Int
    val modifier: Int

    def dealDamage(): Int = {
      diceDamage() + modifier
    }

  }

  case class Axe(toHit: Int = 5, diceDamage: () => Int = rollDice(1,12), modifier: Int = 0) extends Weapon{
    def displayName: String = "Axe"
  }

  case class Scimitar(toHit: Int = 4, diceDamage: () => Int = rollDice(1,6), modifier: Int = 2) extends Weapon{
    def displayName: String = "Scimitar"
  }

  case class ShortSword(toHit: Int = 5, diceDamage: () => Int = rollDice(1,6), modifier: Int = 3) extends Weapon{
    def displayName: String = "Short Sword"
  }

  case class Sword(toHit: Int = 5, diceDamage: () => Int = rollDice(1,6), modifier: Int = 4) extends Weapon{
    def displayName: String = "Sword"
  }

}
