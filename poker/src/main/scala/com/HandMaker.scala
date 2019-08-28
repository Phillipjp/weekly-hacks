package com

import com.Domain.Hand
import com.CardValue._
import com.HandRanks._

object HandMaker {

  def makeFlush(cards: Seq[Card]): Hand = {

    val flushSuit = cards.groupBy(c => c.suit).mapValues(_.size).filter{case(_, size) => size > 4}.head._1
    val scoringCards = cards.filter(card => card.suit == flushSuit).sortBy(-_.value.id).take(5)
    val kickers = cards.filter(card => !scoringCards.contains(card)).sortBy(-_.value.id)
    val royal = Seq(Ace, King, Queen, Jack, Ten)
    if(scoringCards.map(card => card.value) == royal)
      Hand(scoringCards, kickers, royalFlushRank)
    else
      Hand(scoringCards, kickers, flushRank)
  }

}