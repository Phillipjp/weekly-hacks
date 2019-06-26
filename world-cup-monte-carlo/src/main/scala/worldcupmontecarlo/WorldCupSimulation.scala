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

  def simulateWorldCup(teams: Seq[Team], simulations: Int, random: Random):  Map[Team, Int] = {
    Stream.continually(teams)
      .take(simulations)
      .map(simulateTournament(_, random))
      .groupBy(team => team)
      .mapValues(_.size)
  }

  def getWinner(results: Map[Team, Int]): Team = {
    results
      .maxBy{case (_, wins) => wins}
      ._1
  }

  def calculateOdds(results: Map[Team, Int]):  Seq[(String, Odds)] = {
    val total = results.values.sum
    results
      .mapValues(wins => Odds(wins, total))
      .toSeq
      .map{ case (team, odds) => (team.name, odds)}
      .sortBy{ case (_, odds) => -odds.decimal()}
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
    val results = worldCup.simulateWorldCup(teams, 1000000, new Random())
    val winner = worldCup.getWinner(results)
    println(winner)
    println()
    val odds = worldCup.calculateOdds(results)
    odds.foreach(println)
  }
}