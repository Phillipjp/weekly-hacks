package com
import com.Domain.Player
import com.Deck._
import com.Dealer._
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

    riverDeck.foreach(println)
  }

}