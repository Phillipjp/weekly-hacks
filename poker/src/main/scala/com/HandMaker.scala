package com

import com.Domain.Hand
import com.CardValue._
import com.HandRanks._

object HandMaker {

  def makeFlush(cards: Seq[Card]): Hand = {

    val flushSuit = cards.groupBy(c => c.suit).mapValues(_.size).filter{case(_, size) => size > 4}.head._1
    val scoringCards = cards.filter(card => card.suit == flushSuit).sortBy(-_.value.id).take(5)
    val kickers = getKickers(cards, scoringCards)
    val royal = Seq(Ace, King, Queen, Jack, Ten)
    if(scoringCards.map(card => card.value) == royal)
      Hand(scoringCards, kickers, royalFlushRank)
    else
      Hand(scoringCards, kickers, flushRank)
  }

  def makeNOfAKind(cards: Seq[Card], n: Int): Hand ={
    val scoringCards: Seq[Card] = scoringCardsForOfAKind(cards, n)
    val kickers = getKickers(cards, scoringCards)
    n match {
      case 4 => Hand(scoringCards, kickers, fourOfAKindRank)
      case 3 => Hand(scoringCards, kickers, threeOfAKindRank)
      case 2 => Hand(scoringCards, kickers, pairRank)
    }
  }

  private def getKickers(cards: Seq[Card], scoringCards: Seq[Card]) =
    cards.filter(card => !scoringCards.contains(card)).sortBy(-_.value.id)


  private def scoringCardsForOfAKind(cards: Seq[Card], n: Int) = {
    cards
      .groupBy(c => c.value.id)
      .filter { case (_, groupedCards) => groupedCards.size == n }
      .toSeq.maxBy { case (cardValue, _) => cardValue }
      ._2
  }
}
