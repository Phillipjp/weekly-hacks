package com
import com.Domain.Hand
import com.HandChecker._
import com.HandMaker._

case class Player(hand: Hand, id: Int) {

  def makeHand(tableCards: Seq[Card]) : Hand = {
    val usableCards = hand.scoringCards ++ hand.kickers ++ tableCards

    if(isRoyalFlush(usableCards))
      makeFlush(usableCards)
    else if(isStraightFlush(usableCards))
      makeStraightFlush(usableCards)
    else if(isNOfAKind(usableCards, 4))
      makeNOfAKind(usableCards, 4)
    else if(isFullHouse(usableCards))
      makeFullHouse(usableCards)
    else if(isFlush(usableCards))
      makeFlush(usableCards)
    else if(isStraight(usableCards))
      makeStraight(usableCards)
    else if(isNOfAKind(usableCards, 3))
      makeNOfAKind(usableCards, 3)
    else if(isTwoPair(usableCards))
      makeTwoPair(usableCards)
    else if(isNOfAKind(usableCards, 2))
      makeNOfAKind(usableCards, 2)
    else
      makeHighCard(usableCards)
  }

}
