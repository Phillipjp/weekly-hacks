package com
import scala.util.Random.shuffle
import com.Domain.{Deck, Player}

import scala.annotation.tailrec


object Dealer {

  private def dealCard(deck: Deck): (Card, Deck) = {
    val topCard = deck.last
    val newDeck = discardCard(deck)
    (topCard, newDeck)
  }

  private def discardCard(deck: Deck): Deck = {
    deck.dropRight(1)
  }

  def shuffleDeck(deck: Deck): Deck ={
    shuffle(deck)
  }

  @tailrec
  def dealToPlayers(deck: Deck, players: Seq[Player], numPlayers: Int): (Seq[Player], Deck) ={
    if(numPlayers == players.length)
      (players, deck)
    else{
      val (cards, newDeck) = dealToPlayer(deck)
      dealToPlayers(newDeck, players :+ Player(cards), numPlayers)
    }

  }

  private def dealToPlayer(deck: Deck): (Seq[Card], Deck) = {
    val deal1 = dealCard(deck)
    val deal2 = dealCard(deal1._2)
    (Seq(deal1._1, deal2._1), deal2._2)
  }


}