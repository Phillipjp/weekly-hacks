package com

import org.scalatest.{FlatSpec, Matchers}
import com.Suit._
import com.CardValue._
import com.HandChecker._

class HandCheckerSpec extends FlatSpec with Matchers {

  it should "correctly identify flushes" in {
    val hand1 = Seq(Card(SPADE, Two),Card(SPADE, Three),Card(SPADE, Four),
      Card(SPADE, Five),Card(SPADE, Six),Card(SPADE, Seven),Card(SPADE, Eight))

    val hand2 = Seq(Card(SPADE, Two),Card(SPADE, Three),Card(SPADE, Four),
      Card(SPADE, Five),Card(SPADE, Six),Card(HEART, Seven),Card(DIAMOND, Eight))

    val hand3 = Seq(Card(SPADE, Two),Card(SPADE, Three),Card(SPADE, Four),
      Card(SPADE, Five),Card(HEART, Six),Card(DIAMOND, Seven),Card(CLUB, Eight))

    isFlush(hand1) should equal(true)
    isFlush(hand2) should equal(true)
    isFlush(hand3) should equal(false)
  }

  it should "correctly identify straights" in {
    val hand1 = Seq(Card(SPADE, Two),Card(SPADE, Three),Card(SPADE, Four),
      Card(SPADE, Five),Card(SPADE, Six),Card(SPADE, Seven),Card(SPADE, Eight))

    val hand2 = Seq(Card(SPADE, Two),Card(SPADE, Three),Card(SPADE, Four),
      Card(SPADE, Five),Card(SPADE, Six),Card(HEART, Seven),Card(DIAMOND, Ace))

    val hand3 = Seq(Card(SPADE, Two),Card(SPADE, Three),Card(SPADE, Four),
      Card(SPADE, Five),Card(HEART, Six),Card(DIAMOND, Ten),Card(CLUB, Ace))

    val hand4 = Seq(Card(SPADE, Six),Card(SPADE, Four),Card(SPADE, Three),
      Card(SPADE, Five),Card(HEART, Two),Card(DIAMOND, Ten),Card(CLUB, Ace))

    val hand5 = Seq(Card(SPADE, Two),Card(SPADE, Three),Card(SPADE, Four),
      Card(SPADE, Five),Card(HEART, Six),Card(DIAMOND, Six),Card(CLUB, Two))

    val hand6 = Seq(Card(SPADE, Two),Card(SPADE, Three),Card(SPADE, King),
      Card(SPADE, Five),Card(HEART, Seven),Card(DIAMOND, Ten),Card(CLUB, Ace))

    val hand7 = Seq(Card(SPADE, Two),Card(SPADE, Three),Card(SPADE, Four),
      Card(SPADE, Five))

    isStraight(hand1) should equal(true)
    isStraight(hand2) should equal(true)
    isStraight(hand3) should equal(true)
    isStraight(hand4) should equal(true)
    isStraight(hand5) should equal(true)
    isStraight(hand6) should equal(false)
    isStraight(hand7) should equal(false)

  }

  it should "correctly identify straight flushes" in {
    val hand1 = Seq(Card(SPADE, Two),Card(SPADE, Three),Card(SPADE, Four),
      Card(SPADE, Five),Card(SPADE, Six),Card(SPADE, Seven),Card(SPADE, Eight))

    val hand2 = Seq(Card(SPADE, Two),Card(SPADE, Three),Card(SPADE, Four),
      Card(SPADE, Five),Card(SPADE, Six),Card(HEART, Seven),Card(DIAMOND, Ace))

    val hand3 = Seq(Card(SPADE, Two),Card(DIAMOND, Three),Card(CLUB, Four),
      Card(HEART, Five),Card(SPADE, Six),Card(DIAMOND, Seven),Card(CLUB, Eight))

    val hand4 = Seq(Card(SPADE, Two),Card(SPADE, Three),Card(SPADE, Four),
      Card(SPADE, Five))

    isStraightFlush(hand1) should equal(true)
    isStraightFlush(hand2) should equal(true)
    isStraightFlush(hand3) should equal(false)
    isStraightFlush(hand4) should equal(false)

  }

  it should "correctly identify royal flushes" in {
    val hand1 = Seq(Card(SPADE, Ace),Card(SPADE, King),Card(SPADE, Queen),
      Card(SPADE, Jack),Card(SPADE, Ten),Card(SPADE, Seven),Card(SPADE, Eight))

    val hand2 = Seq(Card(CLUB, Ace),Card(SPADE, King),Card(DIAMOND, Queen),
      Card(HEART, Jack),Card(SPADE, Ten),Card(SPADE, Seven),Card(SPADE, Eight))

    val hand3 = Seq(Card(SPADE, Two),Card(SPADE, Three),Card(SPADE, Four),
      Card(SPADE, Five),Card(SPADE, Six),Card(SPADE, Seven),Card(SPADE, Eight))

    isRoyalFlush(hand1) should equal(true)
    isRoyalFlush(hand2) should equal(false)
    isRoyalFlush(hand3) should equal(false)

  }

  it should "correctly identify n of a kind" in {
    val hand1 = Seq(Card(SPADE, Two),Card(DIAMOND, Two),Card(CLUB, Two), Card(HEART, Two))

    val hand2 = Seq(Card(SPADE, Two),Card(DIAMOND, Two),Card(CLUB, Two))

    val hand3 = Seq(Card(SPADE, Two),Card(DIAMOND, Two))

    val hand4 = Seq(Card(SPADE, Two),Card(DIAMOND, Two),Card(CLUB, Two), Card(HEART, Three))

    isNOfAKind(hand1, 4) should equal(true)
    isNOfAKind(hand2, 3) should equal(true)
    isNOfAKind(hand3, 2) should equal(true)
    isNOfAKind(hand4, 4) should equal(false)
  }

  it should "correctly identify a full house" in {
    val hand1 = Seq(Card(SPADE, Two),Card(DIAMOND, Two),Card(CLUB, Three), Card(HEART, Three), Card(HEART, Three))

    val hand2 = Seq(Card(SPADE, Two),Card(DIAMOND, Two),Card(CLUB, Two))

    val hand3 = Seq(Card(SPADE, Two),Card(DIAMOND, Two))

    isFullHouse(hand1) should equal(true)
    isFullHouse(hand2) should equal(false)
    isFullHouse(hand3) should equal(false)
  }

}
