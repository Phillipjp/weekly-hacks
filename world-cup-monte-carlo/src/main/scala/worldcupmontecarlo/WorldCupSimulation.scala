package worldcupmontecarlo
import scala.util.Random


class WorldCupSimulation {

  def simulateMatch(teamA: Team, teamB: Team, random: Random): Team ={
    val probA = winProbability(teamA.rating, teamB.rating)
    val rand = random.nextDouble()
    if(rand > probA)
      teamB
    else
      teamA
  }

  private def winProbability(ratingA: Double, ratingB: Double): Double ={
    val diff = (ratingB - ratingA)/400
    1/(1 + Math.pow(10, diff))
  }

  def simulateRound(teams: Seq[Team], random: Random): Seq[Team] ={
    teams.grouped(2).map((teams: Seq[Team]) => simulateMatch(teams.head, teams.last, random)).toSeq
  }

  def simulateTournament(teams: Seq[Team], random: Random): Team ={
    if(teams.length == 1)
      teams.head
    else {
      val teamsLeft = simulateRound(teams, random)
      simulateTournament(teamsLeft, random)
    }
  }

  def simulateWorldCup(teams: Seq[Team], simulations: Int, random: Random): Team = {
    Stream.continually(teams)
      .take(simulations)
      .map(simulateTournament(_, random))
      .groupBy(team => team)
      .mapValues(_.size)
      .maxBy{case (_, wins) => wins}
      ._1
  }

  def calculateOdds(teams: Seq[Team], simulations: Int, random: Random):  Seq[(String, String)] = {
    Stream.continually(teams)
      .take(simulations)
      .map(simulateTournament(_, random))
      .groupBy(team => team)
      .mapValues(wins => odds.fractional(wins.size, simulations))
      .toSeq
      .map{ case (team, odds) => (team.name, odds)}
      .sortBy{ case (_, fraction) => -odds.fractionalToDecimal(fraction)}

  }
}

object odds {

  private def calculateGCD(a: Int, b: Int): Int = {
    if(a == 0)
      b
    else if(b == 0)
      a
    else
      calculateGCD(b, a % b)
  }

  def fractional(a: Int, b: Int): String = {

    val gcd = calculateGCD(a, b)
    val numerator = a/gcd
    val denominator = b/gcd

    numerator + "/" + denominator
  }

  def decimal(numerator: Double, denominator: Double): Double = {
    numerator/denominator
  }

  def fractionalToDecimal(fraction: String): Double = {
    val numerator = fraction.split("/").head.toDouble
    val denominator = fraction.split("/").last.toDouble

    decimal(numerator, denominator)

  }
}

case class Team(name: String, rating: Int)

object WorldCupSimulation {
  def main(args: Array[String]): Unit = {
    val teams = Seq(
      Team("Norway", 1915),
      Team("Australia", 2003),
      Team("England", 2049),
      Team("Cameroon", 1499),
      Team("France", 2043),
      Team("Brazil", 1944),
      Team("Spain", 1913),
      Team("USA", 2101),
      Team("Italy", 1868),
      Team("China", 1866),
      Team("Netherlands", 1967),
      Team("Japan", 1991),
      Team("Germany", 2072),
      Team("Nigeria", 1599),
      Team("Sweden", 1962),
      Team("Canada", 2006)
    )
    val worldCup = new WorldCupSimulation
    val winner = worldCup.simulateWorldCup(teams, 1000000, new Random())
    println(winner)
    println()
    val odds = worldCup.calculateOdds(teams, 1000000, new Random())
    odds.foreach(println)
  }
}