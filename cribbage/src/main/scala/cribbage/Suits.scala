package cribbage

object Suits {

  sealed trait Suit{
    val suit: String
  }

  case object Spades extends Suit{
    override val suit: String = "SPADES"
  }

  case object Diamonds extends Suit{
    override val suit: String = "DIAMONDS"
  }

  case object Clubs extends Suit{
    override val suit: String = "CLUBS"
  }

  case object Hearts extends Suit{
    override val suit: String = "HEARTS"
  }

  val suits = Seq(Spades, Diamonds, Clubs, Hearts)

}
