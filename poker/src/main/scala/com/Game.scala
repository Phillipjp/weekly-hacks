package com
import com.Dealer._
import com.Deck._
import com.Domain.{Hand, PlayerHand}
import com.HandComparator._
import com.Suit._
import com.CardValue._

object Game {



  def main(args: Array[String]): Unit = {

    val deck = shuffleDeck(createDeck())

    val (players, newDeck) = dealToPlayers(deck, Seq(), 4)

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

    val hands = players.map(player => PlayerHand(player.makeHand(table), player.id))
    hands.foreach(println)

    val winningHands = getWinningPlayerHand(hands)
    if(winningHands.length == 1){
      println(s"Player ${winningHands.head.id} wins!")
    }
    else{
      println(s"Players ${winningHands.map(wh => wh.id).mkString(" and ")} draw!")
    }


  }

}