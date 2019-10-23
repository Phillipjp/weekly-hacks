package roguelike

import roguelike.Dice.rollDice
import roguelike.Enchantments._

object Weapons{

  sealed trait Weapon extends Displayable {

    val toHit: Int
    val diceDamage: () => Int
    val modifier: Int
    val enchantment: Enchantment

    def dealDamage(): Int = {
      diceDamage() + modifier + enchantment.damageModifier
    }

  }

  case class Axe(toHit: Int = 5, diceDamage: () => Int = rollDice(1,12), modifier: Int = 0, enchantment: Enchantment = getEnchantment) extends Weapon{
    def displayName: String = s"${enchantment.displayName}Axe"
  }

  case class Scimitar(toHit: Int = 4, diceDamage: () => Int = rollDice(1,6), modifier: Int = 2, enchantment: Enchantment = getEnchantment) extends Weapon{
    def displayName: String = s"${enchantment.displayName}Scimitar"
  }

  case class ShortSword(toHit: Int = 5, diceDamage: () => Int = rollDice(1,6), modifier: Int = 3, enchantment: Enchantment = getEnchantment) extends Weapon{
    def displayName: String = s"${enchantment.displayName}Short Sword"
  }

  case class Sword(toHit: Int = 5, diceDamage: () => Int = rollDice(1,6), modifier: Int = 4, enchantment: Enchantment = getEnchantment) extends Weapon{
    def displayName: String = s"${enchantment.displayName}Sword"
  }

}
