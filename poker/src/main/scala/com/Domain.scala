package com

object Domain {

  type Deck = Seq[Card]

  case class Player(cards: Seq[Card])

  case class Hand(scoringCards: Seq[Card], kickers: Seq[Card], rank: Int)

}

object HandRanks {
  val royalFlushRank = 10
  val straightFlushRank = 9
  val fourOfAKindRank = 8
  val fullHouseRank = 7
  val flushRank = 6
  val straightRank = 5
  val threeOfAKindRank = 4
  val twoPairRank = 3
  val pairRank = 2
  val highCardRank = 1
}