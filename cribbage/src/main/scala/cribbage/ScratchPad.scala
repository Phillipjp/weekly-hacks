package cribbage

import scala.annotation.tailrec
import scala.util.Random

object ScratchPad {

  sealed trait Rank{
    val rank: String
    val value: Int
  }

  case object Ace extends Rank {
    override val rank: String = "ACE"
    override val value: Int = 1
  }

  case object Two extends Rank {
    override val rank: String = "TWO"
    override val value: Int = 2
  }

  case object Three extends Rank {
    override val rank: String = "THREE"
    override val value: Int = 3
  }

  case object Four extends Rank {
    override val rank: String = "FOUR"
    override val value: Int = 4
  }

  case object Five extends Rank {
    override val rank: String = "FIVE"
    override val value: Int = 5
  }

  case object Six extends Rank {
    override val rank: String = "SIX"
    override val value: Int = 6
  }

  case object Seven extends Rank {
    override val rank: String = "SEVEN"
    override val value: Int = 7
  }

  case object Eight extends Rank {
    override val rank: String = "EIGHT"
    override val value: Int = 8
  }

  case object Nine extends Rank {
    override val rank: String = "NINE"
    override val value: Int = 9
  }

  case object Ten extends Rank {
    override val rank: String = "TEN"
    override val value: Int = 10
  }

  case object Jack extends Rank {
    override val rank: String = "QUEEN"
    override val value: Int = 10
  }

  case object Queen extends Rank {
    override val rank: String = "QUEEN"
    override val value: Int = 10
  }

  case object King extends Rank {
    override val rank: String = "KING"
    override val value: Int = 10
  }

  val ranks = Seq(Ace, Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen, King)

  sealed trait Suit{
    val suit: String
  }

  case object Spades extends Suit{
    override val suit: String = "SPADES"
  }

  case object Diamonds extends Suit{
    override val suit: String = "DIAMONDS"
  }

  case object Clubs extends Suit{
    override val suit: String = "CLUBS"
  }

  case object Hearts extends Suit{
    override val suit: String = "HEARTS"
  }

  val suits = Seq(Spades, Diamonds, Clubs, Hearts)

  case class Card(rank: Rank, suit: Suit)

  case class Deck(deck: Seq[Card]){

    def dealCard: Card = {
    deck.head
    }
  }

  object Dealer{

    def makeDeck: Deck = {
      val deck = suits.flatMap(suit => ranks.map(rank => Card(rank, suit)))
      Deck(deck)
    }

    def shuffleDeck(deck: Deck): Deck = {
      deck.copy(deck = Random.shuffle(deck.deck))
    }

    def deal(deck: Deck): (Card, Deck) = {
      (deck.dealCard, deck.copy(deck.deck.tail))
    }

  }

  case class Hand(cards: Seq[Card], faceUpCard: Card)

  object Scorer{

    def scoreForSummingToFifteen(hand: Hand): Int = {
      val cards = hand.cards :+ hand.faceUpCard
      val scoringCombos = (2 to 5).map(i => cards.combinations(i).flatten.map(c => c.rank.value).sum).filter(s => s == 15)
      2 * scoringCombos.length
    }

    def scoreForRun(hand: Hand, runLength: Int): Int = {
      val cards = hand.cards :+ hand.faceUpCard
      val scoringRuns = cards.combinations(runLength).filter(c => c.sortBy(_.rank.value).sliding(runLength, 1).exists(isConsecutive))
      runLength * scoringRuns.length
    }

     @tailrec
     private def isConsecutive(cards: Seq[Card]): Boolean ={
      if(cards.size == 1)
        true
      else {
        if(cards.head.rank.value - 1 == cards(1).rank.value)
          isConsecutive(cards.tail)
        else
          false
      }
    }

    def scoreForPairs(hand: Hand): Int = {
      val cards = hand.cards :+ hand.faceUpCard
      val pairScores = Map(1 -> 0, 2 -> 2, 3 -> 6, 4 -> 12)
      cards.groupBy(c => c.rank).map{case(rank, cards) => cards.length}.map(i => pairScores(i)).sum
    }

    def scoreForFlush(hand: Hand): Int = {
      val cards = hand.cards :+ hand.faceUpCard
      if(cards.groupBy(c => c.suit).values.exists(flush => flush.length == 5)){
        5
      }
      else if(hand.cards.groupBy(c => c.suit).values.exists(flush => flush.length == 4)){
        4
      }
      else {
        0
      }
    }

    def scoreForNobs(hand: Hand): Int = {
      if(hand.cards.contains(Card(Jack, hand.faceUpCard.suit))){
        -1
      }
      else{
        0
      }
    }

  }



}
