package com

import scala.annotation.tailrec

object HandComparator {

  def getWinningPlayerHand(players: Seq[Player]): Seq[Player] = {
    val topHands = players.groupBy(_.hand.rank).maxBy{case (rank, _) => rank}._2
    if(topHands.length == 1)
      topHands
    else
      resolveDraw(topHands)
  }

  private def resolveDraw(players: Seq[Player]) : Seq[Player] = {
    val topScoringCardsHands = resolveUsingScoringCards(players, 0)
    if(topScoringCardsHands.length == 1 || topScoringCardsHands.head.hand.scoringCards.size == 5)
      topScoringCardsHands
    else {
      val kickersToUse = 5 - topScoringCardsHands.head.hand.scoringCards.size
      resolveUsingKickers(topScoringCardsHands, kickersToUse, 0)
    }
  }

  @tailrec
  private def resolveUsingScoringCards(players: Seq[Player], index: Int): Seq[Player] = {
    if(index == players.head.hand.scoringCards.length)
      players
    else{
      val topHands = players.groupBy(player => player.hand.scoringCards(index).value.id)
        .maxBy{case (rank, _) => rank}._2
      if(topHands.length == 1)
        topHands
      else
        resolveUsingScoringCards(topHands, index+1)
    }
  }

  @tailrec
  private def resolveUsingKickers(players: Seq[Player], kickersToUse: Int, index: Int): Seq[Player] = {
    if(index == kickersToUse)
      players
    else{
      val topHands = players.groupBy(player => player.hand.kickers(index).value.id)
        .maxBy{case (rank, _) => rank}._2
      if(topHands.length == 1)
        topHands
      else
        resolveUsingKickers(topHands, kickersToUse, index+1)
    }
  }
}
