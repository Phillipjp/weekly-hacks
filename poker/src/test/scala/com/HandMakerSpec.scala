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
      Card(SPADE, Ten),Card(SPADE, King),Card(SPADE, Seven),Card(SPADE, Eight))

    makeFlush(cards1) should equal(Hand(Seq(Card(SPADE, Eight),Card(SPADE, Seven),Card(SPADE, Six),Card(SPADE, Five),Card(SPADE, Four)),
      Seq(Card(SPADE, Three),Card(SPADE, Two)), flushRank))

    makeFlush(cards2) should equal(Hand(Seq(Card(SPADE, Ace),Card(SPADE, King),Card(SPADE, Queen),Card(SPADE, Jack),Card(SPADE, Ten)),
      Seq(Card(SPADE, Eight),Card(SPADE, Seven)), royalFlushRank))
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
}
