package com

import com.Domain.Deck

object Deck {

  def createDeck(): Deck = {
    val orderedDeck = Stream.iterate(Option(Card(Suit.HEART, CardValue.Two)))(nextCard)
      .takeWhile(card => card.isDefined)
    orderedDeck.map(_.get)
  }

  private def nextCard(card: Option[Card]): Option[Card] = {
    card match {
      case Some(c) =>
        CardValue.nextOf (c.value) match {
          case None => Suit.nextOf (c.suit) match {
            case Some (suit) => Option (Card (suit, CardValue.Two) )
            case None => None
          }
          case Some (value) => Option (Card (c.suit, value) )
        }
    }
  }

}
