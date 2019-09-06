package com

import com.CardValue._
import com.Suit._
import com.HandMaker._
import com.Domain.Hand
import com.HandRanks._
import org.scalatest.{FlatSpec, Matchers}

class HandMakerSpec extends FlatSpec with Matchers {


  it should "make a flush hand" in {
    val cards1 = Seq(Card(SPADE, Two),Card(SPADE, Three),Card(SPADE, Four),
      Card(SPADE, Five),Card(SPADE, Six),Card(SPADE, Seven),Card(SPADE, Eight))

    val cards2 = Seq(Card(SPADE, Queen),Card(SPADE, Jack),Card(SPADE, Ace),
      Card(SPADE, Ten),Card(SPADE, King),Card(SPADE, Nine),Card(SPADE, Eight))

    makeFlush(cards1) should equal(Hand(Seq(Card(SPADE, Eight),Card(SPADE, Seven),Card(SPADE, Six),Card(SPADE, Five),Card(SPADE, Four)),
      Seq(Card(SPADE, Three),Card(SPADE, Two)), flushRank))

    makeFlush(cards2) should equal(Hand(Seq(Card(SPADE, Ace),Card(SPADE, King),Card(SPADE, Queen),Card(SPADE, Jack),Card(SPADE, Ten)),
      Seq(Card(SPADE, Nine),Card(SPADE, Eight)), royalFlushRank))
  }

  it should "make an N of a kind hand" in {
    val cards1 = Seq(Card(SPADE, Two), Card(DIAMOND, Two), Card(CLUB, Two), Card(HEART, Two), Card(SPADE, Three), Card(DIAMOND, Three))

    val cards2 = Seq(Card(SPADE, Two), Card(DIAMOND, Two), Card(CLUB, Two), Card(HEART, Four), Card(SPADE, Three), Card(DIAMOND, Three))

    val cards3 = Seq(Card(SPADE, Two), Card(DIAMOND, Two), Card(CLUB, Three), Card(HEART, Four), Card(SPADE, Five), Card(DIAMOND, Six))

    val cards4 = Seq(Card(SPADE, Two), Card(DIAMOND, Two), Card(CLUB, Three), Card(HEART, Three), Card(SPADE, Five), Card(DIAMOND, Six))

    makeNOfAKind(cards1, 4) should equal(Hand(Seq(Card(SPADE, Two), Card(DIAMOND, Two), Card(CLUB, Two), Card(HEART, Two)), Seq(Card(SPADE, Three), Card(DIAMOND, Three)), fourOfAKindRank))
    makeNOfAKind(cards2, 3) should equal(Hand(Seq(Card(SPADE, Two), Card(DIAMOND, Two), Card(CLUB, Two)), Seq(Card(HEART, Four), Card(SPADE, Three), Card(DIAMOND, Three)), threeOfAKindRank))
    makeNOfAKind(cards3, 2) should equal(Hand(Seq(Card(SPADE, Two), Card(DIAMOND, Two)), Seq(Card(DIAMOND, Six), Card(SPADE, Five), Card(HEART, Four), Card(CLUB, Three)), pairRank))
    makeNOfAKind(cards4, 2) should equal(Hand(Seq(Card(CLUB, Three), Card(HEART, Three)), Seq(Card(DIAMOND, Six), Card(SPADE, Five),Card(SPADE, Two), Card(DIAMOND, Two)), pairRank))

  }

  it should "make a full house hand" in{
    val cards1 = Seq(Card(SPADE, Two), Card(DIAMOND, Two), Card(CLUB, Two), Card(HEART, Three), Card(SPADE, Three), Card(DIAMOND, Four))
    val cards2 = Seq(Card(SPADE, Two), Card(DIAMOND, Two), Card(CLUB, Two), Card(HEART, Three), Card(SPADE, Three), Card(DIAMOND, Three))

    makeFullHouse(cards1) should equal(Hand(Seq(Card(SPADE, Two), Card(DIAMOND, Two), Card(CLUB, Two), Card(HEART, Three), Card(SPADE, Three)), Seq(Card(DIAMOND, Four)), fullHouseRank))
    makeFullHouse(cards2) should equal(Hand(Seq(Card(HEART, Three), Card(SPADE, Three), Card(DIAMOND, Three), Card(DIAMOND, Two), Card(CLUB, Two)), Seq(Card(SPADE, Two)), fullHouseRank))

  }

  it should "make a two pair hand" in {
    val cards1 = Seq(Card(SPADE, Two), Card(DIAMOND, Two), Card(CLUB, Three), Card(HEART, Three), Card(SPADE, Five), Card(DIAMOND, Four))
    val cards2 = Seq(Card(SPADE, Two), Card(DIAMOND, Two), Card(CLUB, Three), Card(HEART, Three), Card(SPADE, Four), Card(DIAMOND, Four))

    makeTwoPair(cards1) should equal(Hand(Seq(Card(CLUB, Three), Card(HEART, Three), Card(SPADE, Two), Card(DIAMOND, Two)), Seq( Card(SPADE, Five), Card(DIAMOND, Four)), twoPairRank))
    makeTwoPair(cards2) should equal(Hand(Seq(Card(SPADE, Four), Card(DIAMOND, Four), Card(CLUB, Three), Card(HEART, Three)), Seq(Card(SPADE, Two), Card(DIAMOND, Two)), twoPairRank))
  }

  it should "make a high card hand" in {
    val cards = Seq(Card(SPADE, Queen),Card(SPADE, Jack),Card(SPADE, Ace),
      Card(SPADE, Ten),Card(SPADE, King),Card(SPADE, Seven),Card(SPADE, Eight))

    makeHighCard(cards) shouldBe Hand(Seq(Card(SPADE, Ace)),
      Seq(Card(SPADE, King), Card(SPADE, Queen),Card(SPADE, Jack),
      Card(SPADE, Ten), Card(SPADE, Eight), Card(SPADE, Seven)), highCardRank)
  }

  it should "make a straight" in {
    val cards = Seq(Card(SPADE, Queen), Card(SPADE, Jack), Card(SPADE, Ace),
      Card(SPADE, Ten),Card(SPADE, King),Card(SPADE, Seven),Card(SPADE, Eight))
    makeStraight(cards) shouldBe Hand(Seq(Card(SPADE, Ace), Card(SPADE, King), Card(SPADE, Queen),
      Card(SPADE, Jack),Card(SPADE, Ten)), Seq(Card(SPADE, Eight), Card(SPADE, Seven)), straightRank)
  }

  it should "make a straight flush" in {
    val cards1 = Seq(Card(SPADE, Queen), Card(SPADE, Jack), Card(SPADE, Nine),
      Card(SPADE, Ten),Card(SPADE, King),Card(SPADE, Seven),Card(SPADE, Eight))
    val cards2 = Seq(Card(CLUB, Queen), Card(SPADE, Jack), Card(SPADE, Nine),
      Card(SPADE, Ten),Card(CLUB, King),Card(SPADE, Seven),Card(SPADE, Eight))


    makeStraightFlush(cards1) shouldBe Hand(Seq(Card(SPADE, King), Card(SPADE, Queen),
      Card(SPADE, Jack),Card(SPADE, Ten), Card(SPADE, Nine)), Seq(Card(SPADE, Eight), Card(SPADE, Seven)), straightFlushRank)
    makeStraightFlush(cards2) shouldBe Hand(Seq(Card(SPADE, Jack),Card(SPADE, Ten), Card(SPADE, Nine), Card(SPADE, Eight), Card(SPADE, Seven)),
      Seq(Card(CLUB, King), Card(CLUB, Queen)), straightFlushRank)
  }

}
