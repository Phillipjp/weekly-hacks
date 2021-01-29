package cribbage

object Ranks {
  sealed trait Rank{
    val rank: String
    val value: Int
    val order: Int
  }

  case object Ace extends Rank {
    override val rank: String = "ACE"
    override val value: Int = 1
    override val order: Int = 1
  }

  case object Two extends Rank {
    override val rank: String = "TWO"
    override val value: Int = 2
    override val order: Int = 2
  }

  case object Three extends Rank {
    override val rank: String = "THREE"
    override val value: Int = 3
    override val order: Int = 3
  }

  case object Four extends Rank {
    override val rank: String = "FOUR"
    override val value: Int = 4
    override val order: Int = 4
  }

  case object Five extends Rank {
    override val rank: String = "FIVE"
    override val value: Int = 5
    override val order: Int = 5
  }

  case object Six extends Rank {
    override val rank: String = "SIX"
    override val value: Int = 6
    override val order: Int = 6
  }

  case object Seven extends Rank {
    override val rank: String = "SEVEN"
    override val value: Int = 7
    override val order: Int = 7
  }

  case object Eight extends Rank {
    override val rank: String = "EIGHT"
    override val value: Int = 8
    override val order: Int = 8
  }

  case object Nine extends Rank {
    override val rank: String = "NINE"
    override val value: Int = 9
    override val order: Int = 9
  }

  case object Ten extends Rank {
    override val rank: String = "TEN"
    override val value: Int = 10
    override val order: Int = 10
  }

  case object Jack extends Rank {
    override val rank: String = "QUEEN"
    override val value: Int = 10
    override val order: Int = 11
  }

  case object Queen extends Rank {
    override val rank: String = "QUEEN"
    override val value: Int = 10
    override val order: Int = 12
  }

  case object King extends Rank {
    override val rank: String = "KING"
    override val value: Int = 10
    override val order: Int = 13
  }

  val ranks = Seq(Ace, Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen, King)
}
