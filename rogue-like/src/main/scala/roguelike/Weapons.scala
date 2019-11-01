package roguelike

import roguelike.Dice.rollDice
import roguelike.Enchantments._
import roguelike.Statuses.{Normal, Status}
import roguelike.WeaponElements._

object Weapons{

  sealed trait Weapon extends Displayable {

    val toHit: Int
    val diceDamage: () => Int
    val modifier: Int
    val enchantment: Enchantment
    val element: WeaponElement

    def dealDamage(): Int = {
      diceDamage() + modifier + enchantment.damageModifier
    }

    def specialAttack(): Status = {
      element.specialAttack
    }

  }

  case class Axe(toHit: Int = 5, diceDamage: () => Int = rollDice(1,12), modifier: Int = 0, enchantment: Enchantment = getEnchantment, element: WeaponElement = getWeaponElement) extends Weapon{
    def displayName: String = s"${enchantment.displayName}Axe${element.displayName}"
  }

  case class Scimitar(toHit: Int = 4, diceDamage: () => Int = rollDice(1,6), modifier: Int = 2, enchantment: Enchantment = getEnchantment, element: WeaponElement = getWeaponElement) extends Weapon{
    def displayName: String = s"${enchantment.displayName}Scimitar${element.displayName}"
  }

  case class ShortSword(toHit: Int = 5, diceDamage: () => Int = rollDice(1,6), modifier: Int = 3, enchantment: Enchantment = getEnchantment, element: WeaponElement = getWeaponElement) extends Weapon{
    def displayName: String = s"${enchantment.displayName}Short Sword${element.displayName}"
  }

  case class Sword(toHit: Int = 5, diceDamage: () => Int = rollDice(1,6), modifier: Int = 4, enchantment: Enchantment = getEnchantment, element: WeaponElement = getWeaponElement) extends Weapon{
    def displayName: String = s"${enchantment.displayName}Sword${element.displayName}"
  }

}
