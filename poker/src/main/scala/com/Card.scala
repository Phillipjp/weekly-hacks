package com

import com.CardValue.CardValue
import com.Suit.Suit

class NextEnum extends Enumeration {

  lazy val nextOf = {
    val list = values.toList
    val map = list.zip(list.tail).toMap
    v:Value => map.get(v)
  }
}

object Suit extends NextEnum {
  type Suit = Value
  val SPADE, DIAMOND, CLUB, HEART = Value
}

object CardValue extends NextEnum {
  type CardValue = Value
  val Ace, Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten,
  Jack, Queen, King = Value
}

case class Card(suit: Suit, value: CardValue) {}
