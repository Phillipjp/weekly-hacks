package cribbage

import cribbage.Ranks._
import cribbage.Suits._
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers

class ScorerSpec extends Matchers with AnyFlatSpecLike {

  behavior of "scoreForSummingToFifteen"

  it should "correctly score for all the combinations that add to 15" in {
    // Given
    val hand1 = Hand(Seq(Card(Seven, Spades), Card(Eight, Spades), Card(Three, Spades), Card(Four, Spades)), Card(Five, Spades))
    val hand2 = Hand(Seq(Card(Five, Spades), Card(Ten, Spades), Card(Jack, Spades), Card(Queen, Spades)), Card(King, Spades))

    // When, Then
    Scorer.scoreForSummingToFifteen(hand1) shouldBe 6
    Scorer.scoreForSummingToFifteen(hand2) shouldBe 8

  }

  it should "correctly score for all the runs within a hand" in {
    // Given
    val hand1 = Hand(Seq(Card(Seven, Spades), Card(Eight, Spades), Card(Three, Spades), Card(Four, Spades)), Card(Five, Spades))
    val hand2 = Hand(Seq(Card(Five, Spades), Card(Ten, Spades), Card(Jack, Spades), Card(Queen, Spades)), Card(King, Spades))
    val hand3 = Hand(Seq(Card(Nine, Spades), Card(Ten, Spades), Card(Jack, Spades), Card(Queen, Spades)), Card(King, Spades))
    val hand4 = Hand(Seq(Card(Two, Spades), Card(Four, Spades), Card(Six, Spades), Card(Eight, Spades)), Card(Ten, Spades))

    // When, Then
    Scorer.scoreForRuns(hand1) shouldBe 3
    Scorer.scoreForRuns(hand2) shouldBe 4
    Scorer.scoreForRuns(hand3) shouldBe 5
    Scorer.scoreForRuns(hand4) shouldBe 0

  }

  it should "correctly score for all the pairs within a hand" in {
    // Given
    val hand1 = Hand(Seq(Card(Eight, Clubs), Card(Eight, Spades), Card(Three, Spades), Card(Four, Spades)), Card(Five, Spades))
    val hand2 = Hand(Seq(Card(Eight, Clubs), Card(Eight, Spades), Card(Eight, Diamonds), Card(Four, Spades)), Card(Five, Spades))
    val hand3 = Hand(Seq(Card(Eight, Clubs), Card(Eight, Spades), Card(Eight, Diamonds), Card(Eight, Clubs)), Card(Five, Spades))
    val hand4 = Hand(Seq(Card(Eight, Clubs), Card(Eight, Spades), Card(Eight, Diamonds), Card(Five, Clubs)), Card(Five, Spades))
    val hand5 = Hand(Seq(Card(Eight, Clubs), Card(Eight, Spades), Card(Nine, Diamonds), Card(Five, Clubs)), Card(Five, Spades))
    val hand6 = Hand(Seq(Card(Seven, Spades), Card(Eight, Spades), Card(Three, Spades), Card(Four, Spades)), Card(Five, Spades))

    // When, Then
    Scorer.scoreForPairs(hand1) shouldBe 2
    Scorer.scoreForPairs(hand2) shouldBe 6
    Scorer.scoreForPairs(hand3) shouldBe 12
    Scorer.scoreForPairs(hand4) shouldBe 8
    Scorer.scoreForPairs(hand5) shouldBe 4
    Scorer.scoreForPairs(hand6) shouldBe 0

  }

  it should "correctly score for a flush within a hand" in {
    // Given
    val hand1 = Hand(Seq(Card(Seven, Spades), Card(Eight, Spades), Card(Three, Spades), Card(Four, Spades)), Card(Five, Spades))
    val hand2 = Hand(Seq(Card(Five, Spades), Card(Ten, Spades), Card(Jack, Spades), Card(Queen, Spades)), Card(King, Clubs))
    val hand3 = Hand(Seq(Card(Nine, Spades), Card(Ten, Spades), Card(Jack, Spades), Card(Queen, Clubs)), Card(King, Spades))

    // When, Then
    Scorer.scoreForFlush(hand1) shouldBe 5
    Scorer.scoreForFlush(hand2) shouldBe 4
    Scorer.scoreForFlush(hand3) shouldBe 0

  }

  it should "correctly score for Nobs in a hand" in {
    // Given
    val hand1 = Hand(Seq(Card(Seven, Spades), Card(Eight, Spades), Card(Three, Spades), Card(Jack, Spades)), Card(Five, Spades))
    val hand2 = Hand(Seq(Card(Five, Spades), Card(Ten, Spades), Card(Six, Spades), Card(Jack, Spades)), Card(King, Clubs))

    // When, Then
    Scorer.scoreForNobs(hand1) shouldBe -1
    Scorer.scoreForNobs(hand2) shouldBe 0

  }



}
