package com

import com.CardValue._
import scala.annotation.tailrec

object HandChecker {

  def isFlush(cards: Seq[Card]): Boolean =
    cards.groupBy(c => c.suit).mapValues(_.size).values.toSeq.exists(_ > 4)

  def isStraight(cards: Seq[Card]): Boolean = {
    val distinctCardValues = cards.map(c => c.value.id).sortBy(-_).distinct
    if(distinctCardValues.size > 4) {
      distinctCardValues.sliding(5, 1).exists(isConsecutive)
    }
    else
      false
  }

  def isStraightFlush(cards: Seq[Card]): Boolean = {
    cards.groupBy(c => c.suit)
      .toSeq
      .map { case (_, groupedCards) => groupedCards.sortBy(_.value.id) }
      .exists(sortedCards => sortedCards.size > 4 && isStraight(sortedCards))
  }

  def isRoyalFlush(cards: Seq[Card]): Boolean = {
    val royal = Set(Ace, King, Queen, Jack, Ten)
    cards.groupBy(c => c.suit)
      .toSeq
      .map { case (_, groupedCards) => groupedCards.map(_.value).toSet}
      .exists(royal.subsetOf(_))
  }

  @tailrec
  def isConsecutive(cards: Seq[Int]): Boolean ={
    if(cards.size == 1)
      true
    else {
      if(cards.head - 1 == cards(1))
        isConsecutive(cards.tail)
      else
        false
    }
  }

  def isNOfAKind(cards: Seq[Card], n: Int): Boolean =
    cards.groupBy(c => c.value).mapValues(_.size).values.toSeq.contains(n)

  def isFullHouse(cards: Seq[Card]): Boolean =
    (isNOfAKind(cards, 3) && isNOfAKind(cards, 2)) || isTwoNOfAKinds(cards, 3)

  def isTwoNOfAKinds(cards: Seq[Card], n: Int): Boolean =
    cards.groupBy(c => c.value).mapValues(_.size).values.count(_ == n) == 2

  def atleastMOccurencesOfNOfAKind(cards: Seq[Card], m: Int, n: Int): Boolean =
    cards.groupBy(c => c.value).mapValues(_.size).values.count(_ == n) >= m

  def isTwoPair(cards: Seq[Card]): Boolean =
    atleastMOccurencesOfNOfAKind(cards, 2, 2)
}
