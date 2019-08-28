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
}
