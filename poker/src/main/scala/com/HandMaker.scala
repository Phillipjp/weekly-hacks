package com

import com.Domain.Hand
import com.CardValue._
import com.HandRanks._
import com.HandChecker._
import com.Suit.Suit

import scala.annotation.tailrec

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
      case true =>
        val threeOfAKind = scoringCardsForNOfAKind(cards, 3)
        val pair = scoringCardsForNOfAKind(filterCards(cards, threeOfAKind), 3).tail
        threeOfAKind ++ pair
      case false =>
        val threeOfAKind = scoringCardsForNOfAKind(cards, 3)
        val pair = scoringCardsForNOfAKind(cards, 2)
        threeOfAKind ++ pair
    }

    val kickers = getKickers(cards, scoringCards)
    Hand(scoringCards, kickers, fullHouseRank)
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

  def makeStraight(cards: Seq[Card]): Hand = {
    val scoringCards = straightScoringCards(cards)
    val kickers = getKickers(cards, scoringCards)
    Hand(scoringCards, kickers, straightRank)
  }

  def makeStraightFlush(cards: Seq[Card]): Hand = {
    val flushCards = cards.groupBy(c => c.suit).filter{case(_, cards) => cards.length > 4}.values.head
    val scoringCards = straightScoringCards(flushCards)
    val kickers = getKickers(cards, scoringCards)
    Hand(scoringCards, kickers, straightFlushRank)
  }

  private def straightScoringCards(cards: Seq[Card]): Seq[Card] = {
    val distinctCardValues = cards.map(c => c.value.id).sortBy(-_).distinct
    val slidingCardValues = distinctCardValues.sliding(5, 1).toSeq
    val highestStraightValues = getHighestStraightValues(slidingCardValues)
    highestStraightValues.map(id => getCardByValueId(id, cards))
  }

  @tailrec
  private def getHighestStraightValues(slidingCardValues: Seq[Seq[Int]]): Seq[Int] = {
    if(slidingCardValues.length == 1)
      slidingCardValues.head
    else if(isConsecutive(slidingCardValues.head))
      slidingCardValues.head
    else
      getHighestStraightValues(slidingCardValues.tail)
  }

  private def getCardByValueId(id: Int, cards: Seq[Card]): Card =
    cards.filter(_.value.id == id).head
}
