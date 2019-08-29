package com

import com.Domain.Hand
import com.CardValue._
import com.HandRanks._
import com.HandChecker._

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
    val scoringCards: Seq[Card] = scoringCardsForNOfAKind(cards, n)
    val kickers = getKickers(cards, scoringCards)
    n match {
      case 4 => Hand(scoringCards, kickers, fourOfAKindRank)
      case 3 => Hand(scoringCards, kickers, threeOfAKindRank)
      case 2 => Hand(scoringCards, kickers, pairRank)
    }
  }

  private def getKickers(cards: Seq[Card], scoringCards: Seq[Card]) =
    filterCards(cards, scoringCards).sortBy(-_.value.id)


  private def filterCards(cards: Seq[Card], scoringCards: Seq[Card]) =
    cards.filter(card => !scoringCards.contains(card))


  private def scoringCardsForNOfAKind(cards: Seq[Card], n: Int) = {
    cards
      .groupBy(c => c.value.id)
      .filter { case (_, groupedCards) => groupedCards.size == n }
      .toSeq.maxBy { case (cardValue, _) => cardValue }
      ._2
  }

  def makeFullHouse(cards: Seq[Card]): Hand = {

    val scoringCards = isTwoNOfAKinds(cards, 3) match {
      case true => makeTwoThreeOfAKindsFullHouseScoringCards(cards)
      case false =>
        val pair = scoringCardsForNOfAKind(cards, 2)
        val threeOfAKind = scoringCardsForNOfAKind(cards, 3)
        threeOfAKind ++ pair
    }

    val kickers = getKickers(cards, scoringCards)
    Hand(scoringCards, kickers, fullHouseRank)
  }

  private def makeTwoThreeOfAKindsFullHouseScoringCards(cards: Seq[Card]): Seq[Card] = {
    val threeOfAKind = scoringCardsForNOfAKind(cards, 3)
    val pair = scoringCardsForNOfAKind(filterCards(cards, threeOfAKind), 3).tail
    threeOfAKind ++ pair
  }

  def makeTwoPair(cards: Seq[Card]): Hand = {
    val pair1 = scoringCardsForNOfAKind(cards, 2)
    val pair2 = scoringCardsForNOfAKind(cards.filter(card => !pair1.contains(card)), 2)
    val scoringCards = pair1 ++ pair2
    val kickers = getKickers(cards, scoringCards)
    Hand(scoringCards, kickers, twoPairRank)
  }

  def makeHighCard(cards: Seq[Card]): Hand = {
    val highCard = Seq(cards.maxBy(_.value.id))
    val kickers = getKickers(cards, highCard)
    Hand(highCard, kickers, highCardRank)
  }
}
