package com

import com.CardValue._
import com.Domain.Hand
import com.HandRanks._
import org.scalatest.{FlatSpec, Matchers}
import com.HandComparator._
import com.Suit._


class HandComparatorSpec extends FlatSpec with Matchers {

  private val player1 = Player(Hand(Seq(Card(SPADE, Ace),Card(SPADE, King),Card(SPADE, Queen),
    Card(SPADE, Jack),Card(SPADE, Ten)), Seq(Card(CLUB, Three), Card(CLUB, Two)), royalFlushRank), 1)

  private val player2 = Player(Hand(Seq(Card(CLUB, Five),Card(SPADE, Five),Card(CLUB, Four),
    Card(SPADE, Four)), Seq(Card(HEART, Six), Card(HEART, Three), Card(HEART, Two)), twoPairRank), 2)

  private val player3 = Player(Hand(Seq(Card(HEART, Five),Card(DIAMOND, Five),Card(HEART, Four),
    Card(DIAMOND, Four)), Seq(Card(CLUB, Six), Card(CLUB, Three), Card(CLUB, Two)), twoPairRank), 3)

  private val player4 = Player(Hand(Seq(Card(SPADE, Nine),Card(CLUB, Nine),Card(SPADE, Eight),
    Card(CLUB, Eight)), Seq(Card(CLUB, Four), Card(CLUB, Three), Card(CLUB, Two)), pairRank), 4)

  private val player5 = Player(Hand(Seq(Card(HEART, Nine),Card(DIAMOND, Nine),Card(HEART, Seven),
    Card(DIAMOND, Seven)), Seq(Card(CLUB, Four), Card(CLUB, Three), Card(CLUB, Two)), pairRank), 5)

  private val player6 = Player(Hand(Seq(Card(HEART, Nine),Card(DIAMOND, Nine),Card(HEART, Seven),
    Card(DIAMOND, Seven)), Seq(Card(CLUB, Five), Card(CLUB, Three), Card(CLUB, Two)), pairRank), 5)

  it should "Pick the winning player when there isn't a draw between scoring cards" in {
    getWinningPlayerHand(Seq(player1, player2, player3, player4, player5)) shouldBe Seq(player1)
  }

  it should "Pick the winning player based on the scoring cards values when the highest scoring hands have the same rank" in {
    getWinningPlayerHand(Seq(player4, player5)) shouldBe Seq(player4)
  }

  it should "Pick the winning player based on the kicker values when the highest scoring hands have the same rank" in {
    getWinningPlayerHand(Seq(player5, player6)) shouldBe Seq(player6)
  }

  it should "Pick the winning players when the two winning hands are identical apart from the suits" in {
    getWinningPlayerHand(Seq(player2, player3, player4, player5)) shouldBe Seq(player2, player3)
  }

}
