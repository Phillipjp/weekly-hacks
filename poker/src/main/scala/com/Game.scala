package com

import scala.collection.immutable

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
//    val pojo = new TestPojo()
//    testString(pojo.getDatasetVersion)


    val testy = Seq(
      ("id1", Seq(Recomendation("rec1", 2), Recomendation("rec2", 5), Recomendation("rec3", 1))),
      ("id2", Seq(Recomendation("rec4", 4), Recomendation("rec5", 3), Recomendation("rec1", 9))),
      ("id3", Seq(Recomendation("rec1", 7), Recomendation("rec2", 5), Recomendation("rec6", 1)))
    )


    val test = testy.toMap.map(r => r._2.map((_, r._1))).flatten.groupBy(_._1.recId)
        .values.map(sumRecs)
    test.foreach(println)

  }

  def sumRecs(recs: Iterable[(Recomendation, String)]): ExplainedRecomendation = {

    val summedRec = recs.map(_._1.score).sum
    val explanationIds = recs.map(exp => Explanation(exp._2, exp._1.score/summedRec)).toSeq.sortBy(-_.score)
    val recId = recs.head._1.recId

    ExplainedRecomendation(recId, summedRec, explanationIds)

  }
}

case class Recomendation(recId: String, score: Double)

case class ExplainedRecomendation(recId: String, score: Double, explanations: Seq[Explanation])

case class Explanation(id: String, score: Double = 0)