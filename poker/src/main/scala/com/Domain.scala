package com

object Domain {

  type Deck = Seq[Card]

  case class Player(cards: Seq[Card])

}