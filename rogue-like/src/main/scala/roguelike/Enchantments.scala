package roguelike

import scala.util.Random

object Enchantments {

  trait Enchantment extends Displayable {
    val damageModifier: Int
  }

  case class Blessed(damageModifier: Int = 1) extends Enchantment {
    override def displayName: String = "Blessed "
  }

  case class Unenchanted(damageModifier: Int = 0) extends Enchantment {
    override def displayName: String = ""
  }

  case class Cursed(damageModifier: Int = -1) extends Enchantment {
    override def displayName: String = "Cursed "
  }

  def getEnchantment: Enchantment = {
    val prob = Random.nextInt(100)
    if (prob < 25) Blessed()
    else if(prob < 50) Cursed()
    else Unenchanted()
  }

}
