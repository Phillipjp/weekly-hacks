package com
import scala.util.Random.shuffle

class Dealer {

  def dealDeck(): Unit ={
    val deck = createDeck()
    deck.foreach(println)
    val (newDeck, c1)  = dealCard(deck)
    println(c1)
    val c2 = dealCard(newDeck)
    println(c2._2)
  }

  def dealCard(deck: Seq[Card]): (Seq[Card], Card) ={
    val topCard = deck.last
    val newDeck = discardCard(deck)
    (newDeck, topCard)
  }

  def discardCard(deck: Seq[Card]): Seq[Card]={
    deck.dropRight(1)
  }

  def createDeck(): Seq[Card] ={
    val orderedDeck = Stream.iterate(Card(Suit.SPADE, CardValue.Ace))(nextCard)
      .takeWhile(card => card != Card(Suit.HEART, CardValue.King))
    shuffle(orderedDeck)
  }

  private def nextCard(card: Card): Card ={
    CardValue.nextOf(card.value) match {
      case None => Suit.nextOf(card.suit) match{
        case Some(suit) => Card(suit, CardValue.Ace)
      }
      case Some(value) =>  Card(card.suit, value )
    }
  }

  def main(args: Array[String]): Unit = {
    dealDeck()
  }


}