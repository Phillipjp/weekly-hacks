package cribbage

import cribbage.Ranks.Jack

import scala.annotation.tailrec

object Scorer{

  def scoreHand(hand: Hand): Int = {

    val scorers: Array[Hand => Int] = Array(scoreForSummingToFifteen, scoreForRuns, scoreForPairs, scoreForFlush, scoreForNobs)
    scorers.map(scorer => scorer(hand)).sum
  }

  def scoreForSummingToFifteen(hand: Hand): Int = {
    val cards = hand.cards :+ hand.faceUpCard
    val scoringCombos = (2 to 5).flatMap(i => cards.combinations(i).map(combo => combo.map(c => c.rank.value).sum)).filter(s => s == 15)
    2 * scoringCombos.length
  }

  private def scoreForRunLength(hand: Hand, runLength: Int): Int = {
    val cards = hand.cards :+ hand.faceUpCard
    val scoringRuns = cards.combinations(runLength).filter(c => c.sortBy(_.rank.order).sliding(runLength, 1).exists(isConsecutive))
    runLength * scoringRuns.length
  }

  //TODO: what about two runs of 3 and a run of 4 where the 2 runs of 3 aren't combinations of the run of 4? i.e a hand with card values 3, 4, 5, 5 ,6
  def scoreForRuns(hand: Hand): Int = {
    val fiveRunScore = scoreForRunLength(hand, 5)
    val fourRunScore = scoreForRunLength(hand, 4)
    val threeRunScore = scoreForRunLength(hand, 3)

    if(fiveRunScore > 0){
      fiveRunScore
    }
    else if(fourRunScore > 0){
      fourRunScore
    }
    else if(threeRunScore > 0){
      threeRunScore
    }
    else{
      0
    }
  }

  @tailrec
  private def isConsecutive(cards: Seq[Card]): Boolean ={
    if(cards.size == 1)
      true
    else {
      if(cards.head.rank.order + 1 == cards(1).rank.order)
        isConsecutive(cards.tail)
      else
        false
    }
  }

  def scoreForPairs(hand: Hand): Int = {
    val cards = hand.cards :+ hand.faceUpCard
    val pairScores = Map(2 -> 2, 3 -> 6, 4 -> 12)
    cards.groupBy(c => c.rank).map{case(_, cards) => cards.length}.map(i => pairScores.getOrElse(i, 0)).sum
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
      1
    }
    else{
      0
    }
  }

}
