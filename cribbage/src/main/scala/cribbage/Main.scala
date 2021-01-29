package cribbage

import cribbage.Ranks.{Ace, Four, Three, Two}
import cribbage.Suits.Spades

object Main {

  def main(args: Array[String]): Unit = {
    val cards = Seq(Card(Ace, Spades), Card(Three, Spades), Card(Four, Spades), Card(Two, Spades))
    val sorted = cards.sortBy(_.rank.value)
    println(sorted)
  }
}
