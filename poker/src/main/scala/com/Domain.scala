package com

object Domain {

  type Deck = Seq[Card]

  case class Player(cards: Seq[Card])

  case class Hand(scoringCards: Seq[Card], kickers: Seq[Card], rank: Int)

}