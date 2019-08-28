package com

import scala.annotation.tailrec

object HandChecker {

  def isFlush(cards: Seq[Card]): Boolean =
    cards.groupBy(c => c.suit).mapValues(_.size).values.toSeq.exists(_ > 4)

  def isStraight(cards: Seq[Card]): Boolean = {
    val distinctCardValues = cards.map(c => c.value.id).sorted.distinct
    if(distinctCardValues.size > 4) {
      distinctCardValues.sliding(5, 1).exists(isConsecutive)
    }
    else
      false
  }

  @tailrec
  private def isConsecutive(cards: Seq[Int]): Boolean ={
    if(cards.size == 1)
      true
    else {
      if(cards.head + 1 == cards(1))
        isConsecutive(cards.tail)
      else
        false
    }
  }

  def isNOfAKind(cards: Seq[Card], n: Int): Boolean =
    cards.groupBy(c => c.value).mapValues(_.size).values.toSeq.contains(n)

  def isFullHouse(cards: Seq[Card]): Boolean = {
    isNOfAKind(cards, 3) && isNOfAKind(cards, 2)
  }
}
