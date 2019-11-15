package roguelike

import roguelike.WeaponElements.WeaponElement

import scala.util.Random

object ElementalAffinities {

  trait ElementalAffinity extends Displayable{

    def elementalAffinityDamageMultiplier(weaponElement: WeaponElement): Double
  }

  case class IceAffinity() extends ElementalAffinity {

    override def displayName: String = "Ice "

    override def elementalAffinityDamageMultiplier(weaponElement: WeaponElement): Double = {
      weaponElement match {
        case _: IceAffinity => 0.5
        case _: FireAffinity => 2
        case _ => 1
      }
    }
  }

  case class FireAffinity() extends ElementalAffinity {

    override def displayName: String = "Fire "

    override def elementalAffinityDamageMultiplier(weaponElement: WeaponElement): Double = {
      weaponElement match {
        case _: FireAffinity => 0.5
        case _: IceAffinity => 2
        case _ => 1
      }
    }
  }

  case class PlainAffinity() extends ElementalAffinity {

    override def displayName: String = ""

    override def elementalAffinityDamageMultiplier(weaponElement: WeaponElement): Double = {
      1
    }
  }

  def getElementalAffinity: ElementalAffinity = {
    val prob = Random.nextInt(100)
    if (prob < 25) IceAffinity()
    else if(prob < 50) FireAffinity()
    else PlainAffinity()
  }

}
