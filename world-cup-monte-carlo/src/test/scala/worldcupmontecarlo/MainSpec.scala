package worldcupmontecarlo

import org.scalatest._

class MainSpec extends FlatSpec with Matchers{

//  it should "simulate match" in {
//    val teamA = Team("USA", 2101)
//    val teamB = Team("Germany", 2072)
//
//    val fixture = Fixture(teamA,teamB)
//
//    println(fixture.simulateMatch())
//  }

  it should "simulate tournament" in {
    val teams = Seq(
      Team("Norway", 1915),
      Team("Australia", 2043),
      Team("England", 2043),
      Team("Cameroon", 2043),
      Team("France", 2043),
      Team("Brazil", 2043),
      Team("Spain", 2043),
      Team("USA", 2043),
      Team("Italy", 2043),
      Team("Sweden", 2043),
      Team("Netherlands", 2043),
      Team("Japan", 2043),
      Team("Germany", 2043),
      Team("Nigeria", 2043),
      Team("Sweden", 2043),
      Team("Canada", 2043)
    )
    val t = new WorldCupSimulation
    val winner = t.simulateTournament(teams)
    println(winner)
  }

}

