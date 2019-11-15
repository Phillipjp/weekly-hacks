package roguelike

import roguelike.Weapons._
import roguelike.Dice.rollDice
import roguelike.ElementalAffinities._
import roguelike.Statuses._


object Races{

  sealed trait Race extends Displayable{
    val hitPoints: Double
    val amourClass: Int
    val weapon: Weapon
    val status: Status
    val elementalAffinity: ElementalAffinity

    def attack() : Int

    def statusEffect(): Race
  }

  trait Healer {
    val healingPotions: Int

    def heal(): Human
  }

  trait Looter {

    def loot(enemy: Race): Race
  }

  val humanHitPoints: Double = 16

  case class Human(hitPoints: Double = humanHitPoints, amourClass: Int = 13, weapon: Weapon = Axe(), gold: Int = 0, healingPotions: Int = 1, status: Status = Normal(), elementalAffinity: ElementalAffinity = getElementalAffinity) extends Race with Healer with Looter {

    def attack(): Int = weapon.dealDamage()

    def displayName: String = elementalAffinity.displayName + "Human"

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
      val newWeapon: Weapon = lootWeapon(enemy)

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

    private def lootWeapon(enemy: Race) = {
      println(s"Do you want to take the ${enemy.displayName}'s ${enemy.weapon.displayName} instead of your ${this.weapon.displayName}? (y/n)")
      val lootWeapon = scala.io.StdIn.readLine().toLowerCase
      if (lootWeapon.equals("y"))
        enemy.weapon
      else
        this.weapon
    }

    override def statusEffect(): Race = {
      val character = status match {
        case burnt: Burnt =>
          val statusDamage = burnt.applyStatusEffect
          println(s"You're burnt. You lost $statusDamage health.")
          this.copy(hitPoints= hitPoints-statusDamage, status=burnt.copy(turnCount = burnt.turnCount-1))
        case frozen: Frozen =>
          val statusDamage = frozen.applyStatusEffect
          println(s"You're frozen. You lost $statusDamage health.")
          this.copy(hitPoints= hitPoints-statusDamage, status=frozen.copy(turnCount = frozen.turnCount-1))
        case shocked: Shocked =>
          println("You're shocked. You can't move and miss a go.")
          this.copy(status=shocked.copy(turnCount = shocked.turnCount-1))
        case _: Normal =>
          this.copy()
      }

      if (character.status.turnCount == 0)
        character.copy(status = Normal())
      else
        character
    }
  }

  case class Goblin(hitPoints: Double = rollDice(1,4), amourClass: Int = 10, weapon: Weapon = Scimitar(), status: Status = Normal(), elementalAffinity: ElementalAffinity = getElementalAffinity) extends Race{

    def attack(): Int = weapon.dealDamage()

    def displayName: String = elementalAffinity.displayName + "Goblin"

    override def statusEffect(): Race = {
      val character = status match {
        case burnt: Burnt =>
          val statusDamage = burnt.applyStatusEffect
          println(s"The $displayName is burnt. It lost $statusDamage health.")
          this.copy(hitPoints= hitPoints-statusDamage, status=burnt.copy(turnCount = burnt.turnCount-1))
        case frozen: Frozen =>
          val statusDamage = frozen.applyStatusEffect
          println(s"The $displayName is frozen. It lost $statusDamage health.")
          this.copy(hitPoints= hitPoints-statusDamage, status=frozen.copy(turnCount = frozen.turnCount-1))
        case shocked: Shocked =>
          println(s"The $displayName is shocked. It can't move and misses a go.")
          this.copy(status=shocked.copy(turnCount = shocked.turnCount-1))
        case _: Normal =>
          this.copy()
      }

      if (character.status.turnCount == 0)
        character.copy(status = Normal())
      else
        character
    }
  }

  case class HobGoblin(hitPoints: Double = rollDice(1,6), amourClass: Int = 10, weapon: Weapon = ShortSword(), status: Status = Normal(), elementalAffinity: ElementalAffinity = getElementalAffinity) extends Race{

    def attack(): Int = weapon.dealDamage()

    def displayName: String = elementalAffinity.displayName + "Hob Goblin"

    override def statusEffect(): Race = {
      val character = status match {
        case burnt: Burnt =>
          val statusDamage = burnt.applyStatusEffect
          println(s"The $displayName is burnt. It lost $statusDamage health.")
          this.copy(hitPoints= hitPoints-statusDamage, status=burnt.copy(turnCount = burnt.turnCount-1))
        case frozen: Frozen =>
          val statusDamage = frozen.applyStatusEffect
          println(s"The $displayName is frozen. It lost $statusDamage health.")
          this.copy(hitPoints= hitPoints-statusDamage, status=frozen.copy(turnCount = frozen.turnCount-1))
        case shocked: Shocked =>
          println(s"The $displayName is shocked. It can't move and misses a go.")
          this.copy(status=shocked.copy(turnCount = shocked.turnCount-1))
        case _: Normal =>
          this.copy()
      }

      if (character.status.turnCount == 0)
        character.copy(status = Normal())
      else
        character
    }
  }

  case class Orc (hitPoints: Double = rollDice(1,10), amourClass: Int = 12, weapon: Weapon = Sword(), status: Status = Normal(), elementalAffinity: ElementalAffinity = getElementalAffinity) extends Race{

    def attack(): Int = weapon.dealDamage()

    def displayName: String = elementalAffinity.displayName + "Orc"

    override def statusEffect(): Race = {
      val character = status match {
        case burnt: Burnt =>
          val statusDamage = burnt.applyStatusEffect
          println(s"The $displayName is burnt. It lost $statusDamage health.")
          this.copy(hitPoints= hitPoints-statusDamage, status=burnt.copy(turnCount = burnt.turnCount-1))
        case frozen: Frozen =>
          val statusDamage = frozen.applyStatusEffect
          println(s"The $displayName is frozen. It lost $statusDamage health.")
          this.copy(hitPoints= hitPoints-statusDamage, status=frozen.copy(turnCount = frozen.turnCount-1))
        case shocked: Shocked =>
          println(s"The $displayName is shocked. It can't move and misses a go.")
          this.copy(status=shocked.copy(turnCount = shocked.turnCount-1))
        case _: Normal =>
          this.copy()
      }

      if (character.status.turnCount == 0)
        character.copy(status = Normal())
      else
        character
    }

  }

}
