package cribbage

import cribbage.Ranks._
import cribbage.Suits._
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers

class ScorerSpec extends Matchers with AnyFlatSpecLike {

  behavior of "scoreForSummingToFifteen"

  private val S2: Card = Card(Two, Spades)
  private val S3: Card = Card(Three, Spades)
  private val S4: Card = Card(Four, Spades)
  private val S5: Card = Card(Five, Spades)
  private val S6: Card = Card(Six, Spades)
  private val S7: Card = Card(Seven, Spades)
  private val S8: Card = Card(Eight, Spades)
  private val S9: Card = Card(Nine, Spades)
  private val S10: Card = Card(Ten, Spades)
  private val SJ: Card = Card(Jack, Spades)
  private val SQ: Card = Card(Queen, Spades)
  private val SK: Card = Card(King, Spades)

  private val C8: Card = Card(Eight, Clubs)
  private val C5: Card = Card(Five, Clubs)
  private val CQ: Card = Card(Queen, Clubs)
  private val CK: Card = Card(King, Clubs)

  private val D8: Card = Card(Eight, Diamonds)
  private val D9: Card = Card(Nine, Diamonds)


  it should "correctly score for all the combinations that add to 15" in {
    // Given
    val hand1 = Hand(Seq(S7, S8, S3, S4), S5)

    val hand2 = Hand(Seq(S5, S10, SJ, SQ), SK)

    // When, Then
    Scorer.scoreForSummingToFifteen(hand1) shouldBe 6
    Scorer.scoreForSummingToFifteen(hand2) shouldBe 8

  }



  it should "correctly score for all the runs within a hand" in {
    // Given
    val hand1 = Hand(Seq(S7, S8, S3, S4), S5)
    val hand2 = Hand(Seq(S5, S10, S10, SQ), SK)
    val hand3 = Hand(Seq(S9, S10, S10, SQ), SK)
    val hand4 = Hand(Seq(S2, S4, S6, S8), S10)

    // When, Then
    Scorer.scoreForRuns(hand1) shouldBe 3
    Scorer.scoreForRuns(hand2) shouldBe 4
    Scorer.scoreForRuns(hand3) shouldBe 5
    Scorer.scoreForRuns(hand4) shouldBe 0

  }

  it should "correctly score for all the pairs within a hand" in {
    // Given
    val hand1 = Hand(Seq(C8, S8, S3, S4), S5)

    val hand2 = Hand(Seq(C8, S8, D8, S4), S5)
    val hand3 = Hand(Seq(C8, S8, D8, C8), S5)
    val hand4 = Hand(Seq(C8, S8, D8, C5), S5)

    val hand5 = Hand(Seq(C8, S8, D9, C5), S5)
    val hand6 = Hand(Seq(S7, S8, S3, Card(Four, Spades)), S5)

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
    val hand1 = Hand(Seq(S7, S8, S3, S4), S5)
    val hand2 = Hand(Seq(S5, S10, S10, SQ), CK)
    val hand3 = Hand(Seq(S9, S10, S10, CQ), SK)

    // When, Then
    Scorer.scoreForFlush(hand1) shouldBe 5
    Scorer.scoreForFlush(hand2) shouldBe 4
    Scorer.scoreForFlush(hand3) shouldBe 0

  }

  it should "correctly score for Nobs in a hand" in {
    // Given
    val hand1 = Hand(Seq(S7, S8, S3, S10), S5)
    val hand2 = Hand(Seq(S5, S10, S6, S10), CK)

    // When, Then
    Scorer.scoreForNobs(hand1) shouldBe 1
    Scorer.scoreForNobs(hand2) shouldBe 0

  }



}
