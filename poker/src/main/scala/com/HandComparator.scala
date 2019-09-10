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
    else
      resolveUsingKickers(topScoringCardsHands, 0)
  }

  private def resolveUsingScoringCards(players: Seq[Player], index: Int): Seq[Player] = {
    players.groupBy(player => player.hand.scoringCards.map(_.value.id).sum).maxBy{case(handVal, _) => handVal}._2
  }

  @tailrec
  private def resolveUsingKickers(players: Seq[Player], index: Int): Seq[Player] = {
    if(index == players.head.hand.kickers.length)
      players
    else{
      val topHands = players.groupBy(player => player.hand.kickers(index).value.id)
        .maxBy{case (rank, _) => rank}._2
      if(topHands.length == 1)
        topHands
      else
        resolveUsingKickers(topHands, index+1)
    }
  }
}
