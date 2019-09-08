package com
import com.Domain.{Deck, Hand}

import scala.annotation.tailrec
import scala.util.Random.shuffle


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

  def dealToPlayers(deck: Deck, players: Seq[Player]): (Seq[Player], Deck) ={
    @tailrec
    def dealRound(deck: Deck, players: Seq[Player], player: Int): (Seq[Player], Deck) ={
      if(player == players.length)
        (players, deck)
      else{
        val (updatedPlayer, newDeck) = dealToPlayer(players(player), deck)
        val updatedPlayers = (players.filter(p => p.id != player +1) :+ updatedPlayer).sortBy(p => p.id)
        dealRound(newDeck,updatedPlayers, player + 1)
      }
    }

    val round1 = dealRound(deck, players, 0)
    dealRound(round1._2, round1._1, 0)

  }

  def dealToPlayer(player: Player, deck: Deck): (Player, Deck) ={
    val (card, newDeck) = dealCard(deck)
    val hand = Hand(player.hand.scoringCards, player.hand.kickers :+ card, player.hand.rank)
    val updatedPlayer = Player(hand, player.id)
    val updatedHand = updatedPlayer.makeHand(Seq())
    (Player(updatedHand, player.id), newDeck)
  }

  def dealFlop(deck: Deck): (Seq[Card], Deck) = {
    dealFaceUpFlopCards(discardCard(deck), Seq())
  }

  @tailrec
  private def dealFaceUpFlopCards(deck: Deck, flop: Seq[Card]): (Seq[Card], Deck) = {
    if(flop.length == 3)
      (flop, deck)
    else{
      val (card, newDeck) = dealCard(deck)
      dealFaceUpFlopCards(newDeck, flop :+ card)
    }
  }

  def dealTurnOrRiver(deck: Deck): (Card, Deck) = {
    dealCard(discardCard(deck))
  }



}