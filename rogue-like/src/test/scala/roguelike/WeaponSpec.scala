package roguelike

import org.scalatest.{FlatSpec, Matchers}
import roguelike.Weapons.{Axe, Scimitar, ShortSword}

class WeaponSpec extends FlatSpec with Matchers {

  it should "deal weapon damage" in {
    val axe = new Axe()
    println(axe.dealDamage())

    val scimitar = new Scimitar()
    println(scimitar.dealDamage())

    val shortSword = new ShortSword()
    println(shortSword.dealDamage())

  }



}
