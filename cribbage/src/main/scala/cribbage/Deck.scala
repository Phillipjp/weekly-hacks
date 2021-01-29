package cribbage

case class Deck(deck: Seq[Card]){

  def dealCard: Card = {
    deck.head
  }
}
