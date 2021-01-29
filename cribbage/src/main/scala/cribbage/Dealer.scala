package cribbage

import scala.util.Random

object Dealer{

  def makeDeck: Deck = {
    val deck = Suits.suits.flatMap(suit => Ranks.ranks.map(rank => Card(rank, suit)))
    Deck(deck)
  }

  def shuffleDeck(deck: Deck): Deck = {
    deck.copy(deck = Random.shuffle(deck.deck))
  }

  def deal(deck: Deck): (Card, Deck) = {
    (deck.dealCard, deck.copy(deck.deck.tail))
  }

}
