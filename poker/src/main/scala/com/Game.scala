package com

object Game {


  def testString(x: String): Unit = {
    if (x == null){
      println("null")
    }
    else {
      println(x)
    }
  }
  def main(args: Array[String]): Unit = {
//    val dealer = new Dealer
//
//    val deck = dealer.createDeck()
//
//    val players: Seq[Player] = Seq.fill(4)(Player(Seq.empty[Card]))
//        .map( x => {
//          val deal1 = dealer.dealCard(deck)
//          val deal2 = dealer.dealCard(deal1._1)
//          Player(Seq(deal1._2, deal2._2))
//        })
//
//    players.foreach(println)
    val pojo = new TestPojo()
    testString(pojo.getDatasetVersion)

  }
}
