package com

import com.Domain._

import scala.annotation.tailrec

object HandComparator {

  def getWinningPlayerHand(playerHands: Seq[PlayerHand]): Seq[PlayerHand] = {
    val topHands = playerHands.groupBy(_.hand.rank).maxBy{case (rank, _) => rank}._2
    if(topHands.length == 1)
      topHands
    else
      resolveDraw(topHands)
  }

  private def resolveDraw(playerHands: Seq[PlayerHand]) : Seq[PlayerHand] = {
    val topScoringCardsHands = resolveUsingScoringCards(playerHands, 0)
    if(topScoringCardsHands.length == 1)
      topScoringCardsHands
    else
      resolveUsingKickers(topScoringCardsHands, 0)
  }

  @tailrec
  private def resolveUsingScoringCards(playerHands: Seq[PlayerHand], index: Int): Seq[PlayerHand] = {
    if(index == playerHands.head.hand.scoringCards.length)
      playerHands
    else{
      val topHands = playerHands.groupBy(playerHand => playerHand.hand.scoringCards(index).value.id)
        .maxBy{case (rank, _) => rank}._2
      if(topHands.length == 1)
        topHands
      else
        resolveUsingScoringCards(topHands, index+1)
    }
  }

  @tailrec
  private def resolveUsingKickers(playerHands: Seq[PlayerHand], index: Int): Seq[PlayerHand] = {
    if(index == playerHands.head.hand.kickers.length)
      playerHands
    else{
      val topHands = playerHands.groupBy(playerHand => playerHand.hand.kickers(index).value.id)
        .maxBy{case (rank, _) => -rank}._2
      if(topHands.length == 1)
        topHands
      else
        resolveUsingKickers(topHands, index+1)
    }
  }

}
