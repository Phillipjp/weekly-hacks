package com
import com.Dealer._
import com.Deck._
import com.Domain.Hand
import com.HandComparator._

object Game {

  def makePlayers(players: Int) : Seq[Player] =
    (1 to players).map(id => Player(Hand(Seq(), Seq(), 0), id))

  def main(args: Array[String]): Unit = {

    val deck = shuffleDeck(createDeck())

    val (players, newDeck) = dealToPlayers(deck, makePlayers(4))

    players.foreach(println)
    println()

    val (flop, flopDeck) = dealFlop(newDeck)
    flop.foreach(println)
    println()

    val (turn, turnDeck) = dealTurnOrRiver(flopDeck)
    println(turn)
    println()

    val (river, riverDeck) = dealTurnOrRiver(turnDeck)
    println(river)
    println()

    val table = flop :+ turn :+ river

    val finalPlayerHands = players.map(player => Player(player.makeHand(table), player.id))
    finalPlayerHands.foreach(println)

    val winningHands = getWinningPlayerHand(finalPlayerHands)
    if(winningHands.length == 1){
      println(s"Player ${winningHands.head.id} wins!")
    }
    else{
      println(s"Players ${winningHands.map(wh => wh.id).mkString(" and ")} draw!")
    }


  }

}