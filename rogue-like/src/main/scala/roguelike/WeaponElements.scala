package roguelike

import roguelike.Statuses._

import scala.util.Random

object WeaponElements {

  trait WeaponElement extends Displayable{

    def specialAttack: Status

  }

  case class Fire() extends WeaponElement{
    override def specialAttack: Status =
      specialAttackStatus(Burnt())

    override def displayName: String = " of Fire"
  }

  case class Ice() extends WeaponElement{
    override def specialAttack: Status =
      specialAttackStatus(Frozen())

    override def displayName: String = " of Ice"
  }

  case class Thunder() extends WeaponElement{
    override def specialAttack: Status =
      specialAttackStatus(Shocked())

    override def displayName: String = " of Thunder"
  }

  case class Standard() extends WeaponElement{
    override def specialAttack: Status = Normal()

    override def displayName: String = ""
  }

  private def specialAttackStatus(status: Status): Status = {
    val prob = Random.nextInt(100)
    if(prob < 25) status
    else Normal()
  }

  def getWeaponElement: WeaponElement = {
    val prob = Random.nextInt(100)
    if (prob < 10) Fire()
    else if(prob < 20) Ice()
    else if(prob < 30) Thunder()
    else Standard()
  }

}
