package worldcupmontecarlo
import scala.util.Random


class WorldCupSimulation {

  def simulateRound(teams: Seq[Team]): Seq[Team] ={
    teams.sliding(2,2).map((teams: Seq[Team]) => Fixture(teams.head, teams.last).simulateMatch()).toSeq
  }

  def simulateTournament(teams: Seq[Team]): Team ={
    if(teams.length == 1)
      teams.head
    else {
      val teamsLeft = simulateRound(teams)
      simulateTournament(teamsLeft)
    }
  }

  def simulateWorldCup(teams: Seq[Team], simulations: Int): Team = {
    Stream.continually(teams)
      .take(simulations)
      .map(simulateTournament)
      .groupBy{case team => team}
      .mapValues(_.size)
      .maxBy{case (_, wins) => wins}
      ._1
  }


}

case class Team(name: String, rating: Int)

case class Fixture(teamA: Team, teamB: Team){

  def simulateMatch(): Team ={
    val probA = winProbability(teamA.rating, teamB.rating)
    val rand = Random.nextDouble()
    if(rand > probA)
      teamB
    else
      teamA
  }

  private def winProbability(ratingA: Double, ratingB: Double): Double ={
    val diff = (ratingB - ratingA)/400
    1/(1 + Math.pow(10, diff))
  }

}

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
    val winner = worldCup.simulateWorldCup(teams, 1000000)
    println(winner)
  }
}