package roguelike

import roguelike.Races.Race

object Statuses {

  trait Status extends Displayable{
    val turnCount: Int
  }

  trait DamageStatus extends Status {
    def applyStatusEffect: Int
  }

  case class Burnt(turnCount: Int = 2) extends DamageStatus{
    override def applyStatusEffect: Int  = {
      Dice.rollDice(1,3)
    }

    override def displayName: String = "Burnt"
  }

  case class Frozen(turnCount: Int = 1) extends DamageStatus{
    override def applyStatusEffect: Int  = {
      Dice.rollDice(1,3)
    }

    override def displayName: String = "Frozen"
  }

  case class Shocked(turnCount: Int = 1) extends Status{

    override def displayName: String = "Shocked"
  }

  case class Normal(turnCount: Int = 1) extends Status{

    override def displayName: String = ""
  }


}
